/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                     University of Edinburgh, UK                       */
/*                       Copyright (c) 1996,1997                         */
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
/*                                                                       */
/*                 Author: Paul Taylor                                   */
/*                   Date: 6 Jan 1998                                    */
/* --------------------------------------------------------------------- */
/*            LPC residual synthesis alternative version                 */
/*                                                                       */
/*************************************************************************/

#include "EST_error.h"
#include "us_synthesis.h"

void make_segment_single_mapping(EST_Relation &source_lab, 
				 EST_Track &source_pm, 
				 EST_Track &target_pm, EST_IVector &map) 
{
    int i = 0;
    int s_i_start, s_i_end, t_i_start, t_i_end;
    EST_Item *s;
    float s_end, s_start, t_end, t_start, f, m;
    map.resize(target_pm.num_frames());
    
    s_start = t_start = 0.0;

    if (target_pm.t(target_pm.num_frames() - 1) < 
	source_lab.tail()->F("end",0))
    {
	EST_warning("Target pitchmarks end before end of target segment "
		    "timings (%f vs %f). Expect a truncated utterance\n",
		    target_pm.t(target_pm.num_frames() - 1),
	            source_lab.tail()->F("end",0.0));
    }

//    cout << "Source_pm" << source_pm.equal_space() << endl << endl;
//    cout << "Target_pm" << target_pm.equal_space() << endl << endl;

    for (s = source_lab.head(); s; s = next(s))
    {
	s_end = s->F("source_end");
	t_end = s->F("end");
	
	s_i_start = source_pm.index_below(s_start);
	s_i_end = source_pm.index_below(s_end);
	t_i_start = target_pm.index_below(t_start);
	t_i_end = target_pm.index_below(t_end);

	// fudge to make sure that at least one frame is available
	if (s_i_end <= s_i_start)
	    s_i_end += 1;
	
//	printf("%d %d %d %d\n", s_i_start, s_i_end, t_i_start, t_i_end);
//	printf("%f %f %f %f\n\n", s_start, s_end, t_start, t_end);
	
	m = float (s_i_end - s_i_start)/ float(t_i_end - t_i_start);
	for (i = t_i_start, f = 0.0; i < t_i_end; ++i, ++f)
            map[i] = EST_NINT(f * m) + s_i_start;

	s_start = s->F("source_end");
	t_start = s->F("end");
    }
    if (i == 0)
	map.resize(0);  // nothing to synthesize
    else
	map.resize(i - 1);
}


void make_linear_mapping(EST_Track &pm, EST_IVector &map) 
{
    map.resize(pm.num_frames());

    for (int i = 0; i < pm.num_frames(); ++i)
	map[i] = i;
}


void us_mapping(EST_Utterance &utt, const EST_String &method)
{
    EST_Relation *source_lab, *target_lab;
    EST_IVector *map;
    EST_Track *source_coef=0, *target_coef=0;

    source_coef = track(utt.relation("SourceCoef")->head()->f("coefs"));
    target_coef = track(utt.relation("TargetCoef")->head()->f("coefs"));
    
    map = new EST_IVector;
    
//    cout << "mapping method: " << method << endl;
    if (method != "segment_single")
	source_lab = utt.relation("SourceSegments");
    target_lab = utt.relation("Segment", 1);

/*    if (method == "segment")
	make_segment_double_mapping(*source_lab, *source_coef, *target_lab, 
			     *target_coef, *map);
    else if (method == "dp_segment")
	make_dp_mapping(*source_lab, *source_coef, *target_lab, 
			     *target_coef, "Match", *map);
			     */
    if (method == "linear")
	make_linear_mapping(*source_coef, *map);
    else if (method == "segment_single")
	make_segment_single_mapping(*target_lab, *source_coef,
			     *target_coef, *map);
    else
	EST_error("Mapping method \"%s\" not found\n", (const char *)method);

    utt.create_relation("US_map");
    EST_Item *item = utt.relation("US_map")->append();
    item->set_val("map", est_val(map));

//    cout << "map: ";
//    for (int i = 0; i < map->n(); ++ i)
//	cout << i << ":" << map->a(i) << "  ";
//    cout << endl;
      
}


void add_wave_to_utterance(EST_Utterance &u, EST_Wave &sig, 
			const EST_String &name)
{
    u.create_relation(name);
    EST_Item *item = u.relation(name)->append();
//    item->set("name", "[waveform]");
    item->set_val("wave", est_val(&sig));
}

void map_to_relation(EST_IVector &map, EST_Relation &r, 
		     const EST_Track &source_pm, 
		     const EST_Track &target_pm)
{
    EST_Item *s, *t, *a=NULL;
    EST_Utterance *u = r.utt();
    int i;

//    cout << "source: " << source_pm;
//    cout << "target: " << target_pm;

    u->create_relation("smap");
    u->create_relation("tmap");

    for (i = 0; i < source_pm.num_frames(); ++i)
    {
	s = u->relation("smap")->append();
	s->set("index", i);
	s->set("end", source_pm.t(i));
    }

    for (i = 0; i < target_pm.num_frames(); ++i)
    {
	s = u->relation("tmap")->append();
	s->set("index", i);
	s->set("end", target_pm.t(i));
    }

    EST_Item *last_s = 0;

    for (s = u->relation("smap")->head(); s; s = next(s))
    {
	int n = s->I("index");
	for (t = u->relation("tmap")->head(); t; t = next(t))
	{
	    if (map(t->I("index")) == n)
	    {
		if (last_s != s)
		    a = u->relation("lmap")->append(s);
		last_s = s;
		a->append_daughter(t);
		t->set("map", n);
	    }
	}
    }
}

/*
void make_segment_double_mapping(EST_Relation &source_lab, 
				 EST_Track &source_pm, 
				 EST_Relation &target_lab,
				 EST_Track &target_pm, EST_IVector &map) 
{
    int i = 0;
    int s_i_start, s_i_end, t_i_start, t_i_end;
    EST_Item *s, *t;
    float s_end, s_start, t_end, t_start, f, m;
    map.resize(target_pm.num_frames());
    
    s_start = t_start = 0.0;

    if (target_pm.t(target_pm.num_frames() - 1) < 
	target_lab.tail()->F("end"))
	EST_warning("Target pitchmarks end before end of target segment "
		    "timings. Expect a truncated utterance.\n");

    for (s = source_lab.head(), t = target_lab.head(); s && t; 
	 s = next(s), t = next(t))
    {
        if (s->S("name") != t->S("name"))
	  cerr << "Warning: Source and Target segment names do not match: "
	       << s->S("name") << " " << t->S("name") << endl;

	s_end = s->F("end");
	t_end = t->F("end");
	
	s_i_start = source_pm.index_below(s_start);
	s_i_end = source_pm.index_below(s->F("end"));
	t_i_start = target_pm.index_below(t_start);
	t_i_end = target_pm.index_below(t->F("end"));

	// fudge to make sure that at least one frame is available
	if (s_i_end <= s_i_start)
	    s_i_end += 1;
	
	// printf("%d %d %d %d\n", s_i_start, s_i_end, t_i_start, t_i_end);
	// printf("%f %f %f %f\n", s_start, s_end, t_start, t_end);
	
	m = float (s_i_end - s_i_start)/ float(t_i_end - t_i_start);
	for (i = t_i_start, f = 0.0; i < t_i_end; ++i, ++f)
            map[i] = EST_NINT(f * m) + s_i_start;

	s_start = s->F("end");
	t_start = t->F("end");
    }
    if (i == 0)
	map.resize(0);  // nothing to synthesize
    else
	map.resize(i - 1);
}


void make_dp_mapping(EST_Relation &source_lab, EST_Track &source_pm, 
		     EST_Relation &target_lab, EST_Track &target_pm, 
		     const EST_String &match_name, EST_IVector &map) 
{
    int i = 0, j;
    int s_i_start, s_i_end, t_i_start, t_i_end;
    EST_Item *s, *t;
    float s_end, s_start, t_end, t_start, f, m, prev_end;
    map.resize(target_pm.num_frames());

    map.fill(-1);
    
    s_start = t_start = 0.0;

    // should really be replaced by feature functions.
    for (prev_end = 0.0, s = source_lab.head(); s; s = next(s))
    {
	s->set("start", prev_end);
	prev_end = s->F("end");
    }

    // should really be replaced by feature functions.
    for (prev_end = 0.0, s = target_lab.head(); s; s = next(s))
    {
	s->set("start", prev_end);
	prev_end = s->F("end");
    }

    if (target_pm.t(target_pm.num_frames() - 1) < 
	target_lab.tail()->F("end", 1))
	EST_warning("Target pitchmarks end before end of target segment "
		    "timings. Expect a truncated utterance.\n");

    for (s = source_lab.head(); s; s = next(s))
    {
	s_start = s->F("start");

	cout << "source: " << *s << endl;

	while (s && (!s->in_relation(match_name)))
	    s = next(s);

	cout << "active source: " << *s << endl;

	s_end = s->F("end");

	cout << "daughter: " << daughter1(s->as_relation(match_name)) << endl;
	cout << "parent: " << parent(s->as_relation(match_name)) << endl;

	t = parent(s->as_relation(match_name));

	cout << "active target: " << *t << endl;

	t_end = t->F("end");
	t_start = t->F("start");

	s_i_start = source_pm.index_below(s_start);
	s_i_end = source_pm.index_below(s->F("end"));
	t_i_start = target_pm.index_below(t_start);
	t_i_end = target_pm.index_below(t->F("end"));

	// fudge to make sure that at least one frame is available
	if (s_i_end <= s_i_start)
	    s_i_end += 1;
	
	printf("%d %d %d %d\n", s_i_start, s_i_end, t_i_start, t_i_end);
	printf("%f %f %f %f\n", s_start, s_end, t_start, t_end);
	
	m = float (s_i_end - s_i_start)/ float(t_i_end - t_i_start);
	for (i = t_i_start, f = 0.0; i < t_i_end; ++i, ++f)
            map[i] = EST_NINT(f * m) + s_i_start;

	cout << endl;

    }
    
    for (i = 0, j = 0; i < target_pm.num_frames(); ++i)
    {
	cout << map(i) << " ";
	if (map(i) != -1)
	{
	    map[j] = map(i);
	    cout << map(j) << " ";
	    target_pm.t(j++) = target_pm.t(i);
	}
    }

    if (j == 0)
	map.resize(0);  // nothing to synthesize
    else
	map.resize(j);
}
*/
