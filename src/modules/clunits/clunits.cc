/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                         Copyright (c) 1998                            */
/*                        All Rights Reserved.                           */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*   1. The code must retain the above copyright notice, this list of    */
/*      conditions and the following disclaimer.                         */
/*   2. Any modifications must be clearly marked as such.                */
/*   3. Original authors' names are not deleted.                         */
/*   4. The authors' names are not used to endorse or promote products   */
/*      derived from this software without specific prior written        */
/*      permission.                                                      */
/*                                                                       */
/*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        */
/*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*             Author :  Alan W Black                                    */
/*             Date   :  April 1998                                      */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*  Yet another unit selection method.                                   */
/*                                                                       */
/*  Using an acoustic measure find the distance between all units in the */
/*  db.  Try to minimise the mean difference between units in a cluster  */
/*  using CART technology, based on features like phonetic and prosodic  */
/*  context.  This gives a bunch of CARTs for each unit type in the db   */
/*  which are acoustically close.  Use these as candidates and optimise  */
/*  a path through them minimising join using a viterbi search.          */
/*                                                                       */
/*  Advantages:                                                          */
/*    requires little or no measurements at selection time               */
/*    allows for clear method of pruning                                 */
/*    no weights need to be generated                                    */
/*    will optimise appropriately with varying numbers of example units  */
/*                                                                       */
/*  Disadvantages:                                                       */
/*    Units can't cross between clusters                                 */
/*                                                                       */
/*  Implementation of Black, A. and Taylor, P. (1997). Automatically     */
/*  clustering similar units for unit selection in speech synthesis      */
/*  Proceedings of Eurospeech 97, vol2 pp 601-604, Rhodes, Greece.       */
/*                                                                       */
/*  postscript: http://www.cstr.ed.ac.uk/~awb/papers/ES97units.ps        */
/*  http://www.cstr.ed.ac.uk/~awb/papers/ES97units/ES97units.html        */
/*                                                                       */
/*  Comments:                                                            */
/*                                                                       */
/*  This is a new implementation using the newer unit selection/signal   */
/*  processing archtecture in festival                                   */
/*                                                                       */
/*  This is still considered experimental code, its doesn't produce      */
/*  reliable high quality synthesis (though does shine occasionally)     */
/*  it is also slow due to optimal coupling being done in a naive way    */
/*                                                                       */
/*=======================================================================*/
#include <stdlib.h>
#include "EST_math.h"
#include "festival.h"
#include "clunits.h"

static void setup_clunits_params();
static EST_VTCandidate *TS_candlist(EST_Item *s,EST_Features &f);
static EST_VTPath *TS_npath(EST_VTPath *p,EST_VTCandidate *c,EST_Features &f);
static float naive_join_cost(const EST_String &u0,
			     const EST_String &u1,
			     EST_Item *s);
static float optimal_couple(const EST_String &u0_name,
			    const EST_String &u1_name,
			    float &u0_move,
			    float &u1_move);
static void cl_parse_diphone_times(EST_Relation &diphone_stream, 
				   EST_Relation &source_lab);

VAL_REGISTER_CLASS_NODEL(vtcand,EST_VTCandidate);

LISP selection_trees = NIL;
LISP clunits_params = NIL;
static int optimal_coupling = 0;
static int extend_selections = 0;
float continuity_weight = 1;

static LISP clunits_select(LISP utt)
{
    // Select units from db using CARTs to index into clustered unit groups
    EST_Utterance *u = get_c_utt(utt);
    EST_Viterbi_Decoder v(TS_candlist,TS_npath,-1);
    v.set_big_is_good(FALSE);  // big is bad

    setup_clunits_params();

    v.initialise(u->relation("Segment"));
    v.search();
    v.result("unit_id");
    v.copy_feature("unit_this_move");
    v.copy_feature("unit_prev_move");

    return utt;
}

static LISP clunits_get_units(LISP utt)
{
    // Create unit stream and loading params
    EST_Utterance *u = get_c_utt(utt);
    EST_Relation *units,*ss;
    EST_Item *s;

    check_cldb();  // make sure there is one loaded

    units = u->create_relation("Unit");
    for (s=u->relation("Segment")->head(); s != 0; s=next(s))
    {
	EST_Item *unit = units->append();
	EST_Item *db_unit = cldb->get_unit(s->f("unit_id"));
	float st,e;
	unit->set_name(s->f("unit_id"));
	unit->set("fileid",db_unit->S("fileid"));
	// These should be modified from the optimal coupling
	if ((prev(s)) && (s->f_present("unit_this_move")))
	    st = s->F("unit_this_move");
	else
	    st = db_unit->F("start");
	if ((next(s)) && (next(s)->f_present("unit_prev_move")))
	    e = next(s)->F("unit_prev_move");
	else
	    e = db_unit->F("end");
	if ((e-st) < 0.011)
	    e = st + 0.011;
	unit->set("start",st);
	unit->set("middle",db_unit->F("start"));
	unit->set("end",e);
	cldb->load_coefs_sig(unit);
//	cout << *unit << endl;
    }

    // Make it look as much like the diphones as possible for
    // the rest of the code
    ss = u->create_relation("SourceSegments");
    for (s = u->relation("Segment")->head(); s != 0 ; s = next(s))
    {
	EST_Item *d = ss->append();
	d->set_name(s->name());
    }

    cl_parse_diphone_times(*units,*ss);

    return utt;
}

static void cl_parse_diphone_times(EST_Relation &diphone_stream, 
				   EST_Relation &source_lab)
{
    EST_Item *s, *u;
    EST_Track *pm;
    int e_frame, m_frame = 0;
    float dur_1 = 0.0, dur_2 = 0.0, p_time;
    float t_time = 0.0, end;
    p_time = 0.0;
    
    for (s = source_lab.head(), u = diphone_stream.head(); u; u = next(u), 
	 s = next(s))
    {
	pm = track(u->f("coefs"));
	if (pm == 0)
	{
	    cerr << "CLUNIT: couldn't get pitchmarks for " << u->name() << endl;
	    festival_error();
	}
	
	e_frame = pm->num_frames() - 1;
	m_frame = u->I("middle_frame");

	dur_1 = pm->t(m_frame);
	dur_2 = pm->t(e_frame) - dur_1;
	
	s->set("end", (dur_1 + p_time));
	p_time = s->F("end") + dur_2;

	end = dur_1 + dur_2 + t_time;
	t_time = end;
	u->set("end", t_time);
    }
    if (s)
	s->set("end", (dur_2 + p_time));
}

static LISP clunits_simple_wave(LISP utt)
{
    // Naive joining of waveforms
    EST_Utterance *u = get_c_utt(utt);
    EST_Wave *w = new EST_Wave;
    EST_Wave *w1 = 0;
    EST_Item *witem = 0;
    EST_Item *s;
    int size,i,k,c;

    for (size=0,s=u->relation("Unit")->head(); s != 0; s = next(s))
	size += wave(s->f("sig"))->num_samples();

    if (u->relation("Unit")->head())
    {   // This will copy the necessary wave features across
	s = u->relation("Unit")->head();
	*w = *(wave(s->f("sig")));
    }
    i = w->num_samples();
    w->resize(size); // its maximum size
    for (s=next(u->relation("Unit")->head()); s; s=next(s))
    {
	w1 = wave(s->f("sig"));
	// Find last zero crossing
	for (c=0; ((i > 0) && (c < 40)); c++,i--)
	    if (((w->a_no_check(i) < 0) && (w->a_no_check(i-1) >= 0)) ||
		((w->a_no_check(i) >= 0) && (w->a_no_check(i-1) < 0)))
		break;
	if (c == 40) i += 40;
	// Find next zero crossing
	for (c=0,k=1; ((k < w1->num_samples()) && (c < 40)); k++,i++)
	    if (((w1->a_no_check(k) < 0) && (w1->a_no_check(k-1) >= 0)) ||
		((w1->a_no_check(k) >= 0) && (w1->a_no_check(k-1) < 0)))
		break;
	if (c == 40) k -= 40;
	for (; k < w1->num_samples(); k++,i++)
	    w->a_no_check(i) = w1->a_no_check(k);
    }
    w->resize(i);

    witem = u->create_relation("Wave")->append();
    witem->set_val("wave",est_val(w));

    return utt;
}

static LISP clunits_windowed_wave(LISP utt)
{
    // windowed join, no prosodic modification
    EST_Utterance *u = get_c_utt(utt);
    EST_Wave *w = new EST_Wave;
    EST_Wave *w1 = 0;
    EST_Track *t1 = 0;
    EST_Item *witem = 0;
    EST_Item *s;
    int size,i,k,wi,samp_idx;
    int width, lwidth;
    float ltime;
    EST_Wave *www=0;

    for (size=0,s=u->relation("Unit")->head(); s != 0; s = next(s))
	size += wave(s->f("sig"))->num_samples();

    if (u->relation("Unit")->head())
    {   // This will copy the necessary wave features across
	s = u->relation("Unit")->head();
	www = wave(s->f("sig"));
	*w = *www;
    }
    w->resize(size); // its maximum size
    wi=0;
    lwidth = width = 0;
    for (s=u->relation("Unit")->head(); s; s=next(s))
    {
	w1 = wave(s->f("sig"));
	t1 = track(s->f("coefs"));
	samp_idx = 0;
	ltime = 0;
	for (i=0; i < t1->num_frames()-1; i++)
	{
	    width = (int)((t1->t(i)-ltime)*w->sample_rate());
	    if ((i==0) && (lwidth != 0))
		width = (width+lwidth)/2;  // not sure if this is worth it
	    ltime = t1->t(i);
	    wi += width;
	    samp_idx += width;
	    for (k=-width; ((k<width)&&((samp_idx+k)<w1->num_samples())) ;k++)
		w->a(wi+k) += 
		    (int)(0.5*(1+cos((PI/(double)(width))*(double)k))*
			w1->a(samp_idx+k));
	}
	lwidth = width;
    }
    w->resize(wi);

    witem = u->create_relation("Wave")->append();
    witem->set_val("wave",est_val(w));

    return utt;
}

static void setup_clunits_params()
{
    // Set up params
    clunits_params = siod_get_lval("clunits_params",
				    "CLUNITS: no parameters set for module");
    optimal_coupling = get_param_int("optimal_coupling",clunits_params,0);
    extend_selections = get_param_int("extend_selections",clunits_params,0);
    continuity_weight = get_param_float("continuity_weight",clunits_params,1);
    selection_trees = 
	siod_get_lval("clunits_selection_trees",
		      "CLUNITS: clunits_selection_trees unbound");
}

static EST_VTCandidate *TS_candlist(EST_Item *s,EST_Features &f)
{
    // Return a list of candidate units for target s
    // Use the appropriate CART to select a small group of candidates
    EST_VTCandidate *all_cands = 0;
    EST_VTCandidate *c;
    LISP tree,group,l,pd;
    float cluster_mean;
    (void)f;

    tree = car(cdr(siod_assoc_str(s->name(),selection_trees)));
    pd = wagon_pd(s,tree);
    if (pd == NIL)
    {
	cerr << "CLUNITS: no predicted class for " << s->name() << endl;
	festival_error();
    }
    group = car(pd);
    cluster_mean = get_c_float(car(cdr(pd)));
    
    for (l=group; l != NIL; l=cdr(l))
    {
	c = new EST_VTCandidate;
	c->name = s->name()+"_"+get_c_string(car(car(l)));
	c->s = s;
	// Mean distance from others in cluster (could be precalculated)
	c->score = get_c_float(car(cdr(car(l))))-cluster_mean;
	c->score *= c->score;
	// Maybe this should be divided by overall mean of set
	// to normalise this figure (?)

	c->next = all_cands;
	all_cands = c;
    }

    if (extend_selections)
    {
	// An experiment, for all candidates of the previous 
	// item whose following is of this phone type, include
	// them as a candidate
	EST_Item *ppp = prev(s);
	if (ppp)
	{
	    EST_VTCandidate *lc = vtcand(ppp->f("unit_cands"));
	    for ( ; lc; lc = lc->next)
	    {
		EST_Item *unit = cldb->get_unit(lc->name);
		EST_String next_name = unit->S("next_unit","");
		if (next_name.before("_") == s->name())
		{  // its the right type so add it
		    c = new EST_VTCandidate;
		    c->name = next_name;
		    c->s = s;
		    c->score = 0; 
		    c->next = all_cands;
		    all_cands = c;
		}
	    }
	}

	s->set_val("unit_cands",est_val(all_cands));
    }
    return all_cands;
}

static EST_VTPath *TS_npath(EST_VTPath *p,EST_VTCandidate *c,EST_Features &f)
{
    // Combine candidate c with previous path updating score 
    // with join cost
    float cost;
    EST_VTPath *np = new EST_VTPath;
    EST_String u0, u1;
    float u0_move=0.0, u1_move=0.0;
    (void)f;

    np->c = c;
    np->from = p;
    if ((p == 0) || (p->c == 0))
	cost = 0;  // nothing previous to join to
    else
    {
	u0 = p->c->name;
	u1 = c->name;
	if (optimal_coupling)
	    cost = optimal_couple(u0,u1,u0_move,u1_move);
	else // naive measure
	    cost = naive_join_cost(u0,u1,c->s);
	np->f.set("unit_prev_move",u0_move); // new (prev) end
	np->f.set("unit_this_move",u1_move); // new start
    }
    cost *= continuity_weight;
    np->state = c->pos;  // "state" is candidate number

    np->f.set("lscore",c->score+cost);
    if (p==0)
	np->score = (c->score+cost);
    else
	np->score = (c->score+cost) + p->score;
    
    return np;
}

static float optimal_couple(const EST_String &u0_name,
			    const EST_String &u1_name,
			    float &u0_move,
			    float &u1_move)
{
    // Find combination cost of u0 to u1, checking for best
    // frame up to n frames back in u0 and u1.
    // Note this checks the u0 with u1's predecessor, which may or may not
    // be of the same type
    EST_Track *u0_cep, *u1_p_cep;
    float dist, best_val;
    int i,eee;
    EST_String u1_p_name;
    int u0_st, u0_end;
    int u1_p_st, u1_p_end;
    int best_u0, best_u1;
    EST_Item *u0, *u1, *u1_p;
    float f;

    u0 = cldb->get_unit(u0_name);
    u0_move = u0->F("end");
    cldb->load_join_coefs(u0);
    u1 = cldb->get_unit(u1_name);
    u1_p_name = ffeature(u1,"prev_unit");
    u1_p = cldb->get_unit(u1_p_name);
    u1_move = ffeature(u1_p,"end");

    if (u1_p == u0)  // they are consecutive
	return 0.0;
    if (u1_p == 0)  // hacky condition
	return 0.0;

    if (u1_p_name.before("_") != u0_name.before("_"))
	f = 1000;
    else
	f = 1;
    cldb->load_join_coefs(u1_p);
    // Get indexes into full cep for utterances rather than sub ceps
    u0_cep = track(u0->f("join_coeffs"));
    u1_p_cep = track(u1_p->f("join_coeffs"));
    u0_end = u0_cep->num_frames();
    u0_st = u0_cep->num_frames()/3;  // up to 66% into previous
    u1_p_end = u1_p_cep->num_frames();
    u1_p_st = u1_p_cep->num_frames()/3;

    best_u0=u0_end;
    best_u1=u1_p_end;
    best_val = HUGE_VAL;

    // Here we look for the best join without sliding the windows
    if ((u0_end-u0_st) < (u1_p_end-u1_p_st))
	eee = u0_end-u0_st;
    else
	eee = u1_p_end-u1_p_st;
    for (i=0; i < eee; i++)
    {
	dist = frame_distance(*u0_cep,i+u0_st,
			      *u1_p_cep,i+u1_p_st,
			      cldb->cweights);
	if (dist < best_val)
	{
	    best_val = dist;
	    best_u0 = i+u0_st;
	    best_u1 = i+u1_p_st;
	}
    }
#if 0
    // This tries *all* possible matches in the pair, its slow
    // and has a tendency to shorten things more than you'd like
    // so we just use the more simple test above.
    int j;
    for (i=u0_st; i < u0_end; i++)
    {
	for (j=u1_p_st; j < u1_p_end; j++)
	{
	    dist = frame_distance(*u0_cep,i,
				  *u1_p_cep,j,
				  cldb->cweights);
	    if (dist < best_val)
	    {
		best_val = dist;
		best_u0 = i;
		best_u1 = j;
	    }
	}
    }
#endif

    u0_move = u0_cep->t(best_u0);
    u1_move = u1_p_cep->t(best_u1);

    return best_val*f;
}

static float naive_join_cost(const EST_String &u0,
			     const EST_String &u1,
			     EST_Item *s)
{
    // A naive join cost, because I haven't ported the info yet
    (void)u0;
    (void)u1;
    EST_Item *unit1, *unit0;

    unit1 = cldb->get_unit(u1);
    unit0 = cldb->get_unit(u0);

    if (unit0 == unit1)
	return 0;
    else if (unit1->S("prev_unit","0") == u0)
	return 0;
    else if (ph_is_silence(s->name()))
	return 0;
    else if (ph_is_stop(s->name()))
	return 0.2;
    else if (ph_is_fricative(s->name()))
	return 0.3;
    else
	return 1.0;
}

void festival_clunits_init(void)
{
    // Initialization for clunits selection
    proclaim_module("clunits");

    gc_protect(&clunits_params);
    gc_protect(&selection_trees);

    festival_def_utt_module("Clunits_Select",clunits_select,
    "(Clunits_Select UTT)\n\
  Select units from current databases using cluster selection method.");

    festival_def_utt_module("Clunits_Get_Units",clunits_get_units,
    "(Clunits_Get_Units UTT)\n\
  Construct Unit relation from the selected units in Segment and extract\n\
  their parameters from the clunit db.");

    festival_def_utt_module("Clunits_Simple_Wave",clunits_simple_wave,
    "(Clunits_Simple_Wave UTT)\n\
  Naively concatenate signals together into a single wave (for debugging).");

    festival_def_utt_module("Clunits_Windowed_Wave",clunits_windowed_wave,
    "(Clunits_Windowed_Wave UTT)\n\
  Use hamming window over edges of units to join them, no prosodic \n\
  modification though.");

    init_subr_1("clunits:load_db",cl_load_db,
    "(clunits:load_db PARAMS)\n\
  Load index file for cluster database and set up params.");

    init_subr_2("acost:build_disttabs",make_unit_distance_tables,
    "(acost:build_disttabs UTTTYPES PARAMS)\n\
  Built matrices of distances between each ling_item in each each list\n\
  of ling_items in uttypes.   Uses acoustic weights in PARAMS and save\n\
  the result as a matrix for later use.");

    init_subr_2("acost:utt.load_coeffs",acost_utt_load_coeffs,
    "(acost:utt.load_coeffs UTT PARAMS)\n\
  Load in the acoustic coefficients into UTT and set the Acoustic_Coeffs\n\
  feature for each segment in UTT.");

    init_subr_3("acost:file_difference",ac_distance_tracks,
    "(acost:file_difference FILENAME1 FILENAME2 PARAMS)\n\
  Load in the two named tracks and find the acoustic difference over all\n\
  based on the weights in PARAMS.");

}
