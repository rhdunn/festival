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
/*  A quick database structure                                           */
/*                                                                       */
/*=======================================================================*/
#include <stdlib.h>
#include <math.h>
#include "festival.h"
#include "EST_FileType.h"
#include "clunits.h"

static void cl_load_catalogue(EST_String &indexfile);

CLDB *cldb = new CLDB;

LISP cl_load_db(LISP params)
{
    EST_String indexfile;
    int i;
    LISP w;

    cldb->params = params;
    
    indexfile = EST_String("") +
	get_param_str("db_dir",params,"./")+
	get_param_str("catalogue_dir",params,"./")+
	get_param_str("index_name",params,"catalogue")+
	    ".catalogue";
    
    cl_load_catalogue(indexfile);

    cldb->cweights.resize(siod_llength(get_param_lisp("join_weights",params,NIL)));
    for (i=0,w=get_param_lisp("join_weights",params,NIL); w; w=cdr(w),i++)
	cldb->cweights[i] = get_c_float(car(w));

    return NIL;
}

static void cl_load_catalogue(EST_String &indexfile)
{
    EST_TokenStream ts;
    EST_EstFileType t;
    EST_Option hinfo;
    EST_String v;
    bool ascii;
    EST_read_status r;
    
    if (((indexfile == "-") ? ts.open(cin) : ts.open(indexfile)) != 0)
    {
	cerr << "CLUNITS: Can't open catalogue file " << indexfile << endl;
	festival_error();
    }

    if (((r = read_est_header(ts, hinfo, ascii, t)) != format_ok) ||
	(t != est_file_index))
    {
	cerr << "CLUNITS: " << indexfile << " is not an indexfile" << endl;
	festival_error();
    }

    EST_Item *ls = 0;
    while(!ts.eof())
    {
	EST_Item *s = new EST_Item;
	s->set_name(ts.get().string());
	s->set("fileid",ts.get().string());
	s->set("start",atof(ts.get().string()));
	s->set("mid",atof(ts.get().string()));
	s->set("end",atof(ts.get().string()));

	if ((ls != 0) &&
	    (ls->f("fileid") == s->f("fileid")) &&
	    (ls->f("end") == s->f("start")))
	{
	    s->set("prev_unit",ls->name());
	    ls->set("next_unit",s->name());
	}
	cldb->index.add(s->name(),s);
	ls = s;
    }
}

void check_cldb()
{
    if (cldb == 0)
    {
	cerr << "CLDB: no database loaded\n";
	festival_error();
    }
}

void cl_maybe_fix_pitch_c0(EST_Track *c)
{
    // If its pitch synchronous, trash the first coefficient with
    // the pitch value, there should be a cleaner way to do this
    int i;
    float ltime = 0;
    
    if (!c->equal_space())
    {
	for (i=0; i < c->num_frames(); i++)
	{
	    c->a_no_check(i,0) = 1/(c->t(i)-ltime);
	    ltime = c->t(i);
	}
    }
}


void CLDB::load_join_coefs(EST_Item *unit)
{
    // Load in the coefficients and signal for this unit.
    EST_String fileid = unit->f("fileid");
    EST_Item *fileitem = get_fileitem(fileid);

    if (unit->f_present("join_coeffs"))
	return;

    if (fileitem == 0)
    {   // even the file isn't here
	fileitem = new EST_Item;
	fileindex.add(fileid,fileitem);
    }
    if (!fileitem->f_present("join_coeffs"))
    {
	EST_Track *join_coeffs = new EST_Track;
	EST_String jc_filename = 
	    EST_String("") +
		get_param_str("db_dir",cldb->params,"./") +
		    get_param_str("coeffs_dir",cldb->params,"wav/") +
			fileid+
  		          get_param_str("coeffs_ext",cldb->params,".dcoeffs");
	if (join_coeffs->load(jc_filename) != format_ok)
	{
	    delete join_coeffs;
	    cerr << "CLUNITS: failed to load join coeffs file " << 
		jc_filename << endl;
	    festival_error();
	} 
	cl_maybe_fix_pitch_c0(join_coeffs);
	fileitem->set_val("join_coeffs",est_val(join_coeffs));
    }
    EST_Track *join_coeffs = track(fileitem->f("join_coeffs"));
    EST_Track *unit_join_coeffs = new EST_Track;

    int pm_start = join_coeffs->index(unit->F("start"));
    int pm_end = join_coeffs->index(unit->F("end"));

    join_coeffs->sub_track(*unit_join_coeffs, pm_start, pm_end-pm_start+1,0);
    unit->set_val("join_coeffs",est_val(unit_join_coeffs));
    
}

void CLDB::load_coefs_sig(EST_Item *unit)
{
    // Load in the coefficients and signal for this unit.
    EST_String fileid = unit->f("fileid");
    EST_Item *fileitem = get_fileitem(fileid);

    if (unit->f_present("sig"))
	return;

    if (fileitem == 0)
    {   // even the file isn't here
	fileitem = new EST_Item;
	fileindex.add(fileid,fileitem);
    }
    if (!fileitem->f_present("sig"))
    {
	EST_Track *track = new EST_Track;
	EST_String coef_filename = 
	    EST_String("") +
		get_param_str("db_dir",cldb->params,"./") +
		    get_param_str("pm_coeffs_dir",cldb->params,"pm/") +
			fileid+
			    get_param_str("pm_coeffs_ext",cldb->params,".pm");
	if (track->load(coef_filename) != format_ok)
	{
	    delete track;
	    cerr << "CLUNITS: failed to load coeffs file " << 
		coef_filename << endl;
	    festival_error();
	}
	fileitem->set_val("coefs",est_val(track));
	
	EST_Wave *sig = new EST_Wave;
	EST_String sig_filename = 
	    EST_String("") +
		get_param_str("db_dir",cldb->params,"./") +
		    get_param_str("sig_dir",cldb->params,"wav/") +
			fileid+
			    get_param_str("sig_ext",cldb->params,".wav");
	if (sig->load(sig_filename) != format_ok)
	{
	    delete sig;
	    cerr << "CLUNITS: failed to load signal file " << 
		sig_filename << endl;
	    festival_error();
	} 
	fileitem->set_val("sig",est_val(sig));
    }
    EST_Track *coeffs = track(fileitem->f("coefs"));
    EST_Wave *sig = wave(fileitem->f("sig"));
    EST_Track u1;
    EST_Wave *unit_sig = new EST_Wave;

    int pm_start = coeffs->index(unit->F("start"));
    int pm_middle = coeffs->index(unit->F("middle"));
    int pm_end = coeffs->index(unit->F("end"));

//    coeffs->sub_track(u1,Gof((pm_start-1),0), pm_end - pm_start + 1);
    coeffs->sub_track(u1,pm_start, pm_end - pm_start + 1,0);
    EST_Track *unit_coeffs = new EST_Track(u1);
    for (int j = 0; j < u1.num_frames(); ++j)
	unit_coeffs->t(j) = u1.t(j) - coeffs->t(Gof((pm_start - 1), 0));

    unit->set_val("coefs",est_val(unit_coeffs));

    if ((pm_middle-pm_start-1) < 1)
	unit->set("middle_frame", 1);
    else
	unit->set("middle_frame", pm_middle - pm_start -1);
    int samp_start = (int)(coeffs->t(Gof((pm_start - 1), 0))
		  * (float)sig->sample_rate());
    int samp_end = (int)(coeffs->t(pm_end + 1)
		     * (float)sig->sample_rate());
//    int samp_start = (int)(unit->F("start")
//		  * (float)sig->sample_rate());
//    int samp_end = (int)(unit->F("end")
//		     * (float)sig->sample_rate());
    sig->sub_wave(*unit_sig,samp_start, samp_end-samp_start+1);
    unit->set_val("sig",est_val(unit_sig));

}

CLDB::CLDB()
{
    gc_protect(&params);
}

static void del_item(void *s) { delete (EST_Item *)s; }
CLDB::~CLDB()
{
    index.clear(del_item);
    fileindex.clear(del_item);
    gc_unprotect(&params);
}

