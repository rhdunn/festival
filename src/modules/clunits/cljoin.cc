/*************************************************************************/
/*                                                                       */
/*                  Language Technologies Institute                      */
/*                     Carnegie Mellon University                        */
/*                         Copyright (c) 1999                            */
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
/*                 Author: Alan W Black                                  */
/*                   Date: July 2001                                     */
/* --------------------------------------------------------------------- */
/*  Some more interesting join methods                                   */
/*       based on the mapping code in UniSyn                             */
/*                                                                       */
/*************************************************************************/

#include "EST_error.h"
#include "us_synthesis.h"

#if 0
static int awb_voiced(EST_Track &pm,int i)
{
    // Hacky guess at voicedness
    return TRUE;
    if ((i < 2) || (i+3 > pm.num_frames()))
	return FALSE;
    else if (((pm.t(i) - pm.t(i-1)) == (pm.t(i+1) - pm.t(i))) &&
	     ((pm.t(i-1) - pm.t(i-2)) == (pm.t(i) - pm.t(i-1))))
	return FALSE;
    else
	return TRUE;
}

static float frame_duration(EST_Track &pm, int i)
{
    if (i <= 0)
	return frame_duration(pm,i+1);
    else if (i >= pm.num_frames())
	return frame_duration(pm,pm.num_frames()-1);
    else
	return pm.t(i)-pm.t(i-1);
}

static float awb_smoothed(EST_Track &pm, int i)
{
    // Returned smoothed pitch period at i

    return (frame_duration(pm,i-4)+
	    frame_duration(pm,i-3)+
	    frame_duration(pm,i-2)+
	    frame_duration(pm,i-1)+
	    frame_duration(pm,i)+
	    frame_duration(pm,i+1)+
	    frame_duration(pm,i+2))/7.0;
}
#endif


static void make_segment_varied_mapping(EST_Relation &source_lab, 
					EST_Track &source_pm, 
					EST_Track &target_pm, 
					EST_IVector &map,
					float dur_impose_factor,
					float f0_impose_factor) 
{
    int n_i, i,j;
    int spp,tpp;
    float n_start, s_start, t_start;
    float n_end, t_end, s_end;
    float n_dur, s_dur, t_dur;
    float s_dur_factor, t_dur_factor;
    float ntime, stime, ttime, n;
    int max_frames;
    EST_Item *u;
    EST_Track ntarget_pm;

    ntarget_pm = target_pm;
    if (target_pm.num_frames() > source_pm.num_frames())
	max_frames = target_pm.num_frames()+100;
    else
	max_frames = source_pm.num_frames()+100;

    ntarget_pm.resize(max_frames,target_pm.num_channels());
    map.resize(max_frames);

    // I don't like there being a pitch period at time 0 in the source
    for (i=1; i < source_pm.num_frames(); i++)
    {
	source_pm.t(i-1) = source_pm.t(i);
	for (j=0; j < source_pm.num_channels(); j++)
	    source_pm.a(i-1,j) = source_pm.a(i,j);
    }
    source_pm.resize(source_pm.num_frames()-1,source_pm.num_channels());

    if (target_pm.t(target_pm.num_frames() - 1) < 
	source_lab.tail()->F("end",0))
    {
	EST_warning("Target pitchmarks end before end of target segment "
		    "timings (%f vs %f). Expect a truncated utterance\n",
		    target_pm.t(target_pm.num_frames() - 1),
	            source_lab.tail()->F("end",0.0));
    }

    n_i = 0;
    s_start = t_start = n_start = 0.0;
    for (u = source_lab.head(); u; u = next(u))
    {
	s_end = u->F("source_end");
	t_end = u->F("end");
	
	s_dur = s_end - s_start;
	t_dur = t_end - t_start;
	// How much of the target do we impose
	n_dur = s_dur + ((t_dur-s_dur)*dur_impose_factor);
	n_end = n_start + n_dur;
	
	s_dur_factor = s_dur / n_dur;
	t_dur_factor = t_dur / n_dur;
/*	printf("s end %f dur %f factor %f t end %f dur %f factor %f\n",
	       s_end, s_dur, s_dur_factor,
	       t_end, t_dur, t_dur_factor); */
	for (n = 0.0; n + n_start < n_end; n_i++)
	{
	    spp = source_pm.index_below((n * s_dur_factor) + s_start);
	    spp++;
	    stime = source_pm.t(spp) - source_pm.t(spp-1);

	    tpp = target_pm.index((n * t_dur_factor) + t_start);
	    ttime = target_pm.t(tpp) - (tpp == 0 ? 0 : target_pm.t(tpp-1));

	    ntime = stime + ((ttime-stime) * f0_impose_factor);

	    n += ntime;
	    if (n+n_start >= n_end)
		break;
	    ntarget_pm.t(n_i) = n_start + n;
            map[n_i] = spp;
/*	    printf("s pp %d time %f %f t pp %d time %f n %f %f %f\n",
		   spp, stime, source_pm.t(spp),
		   tpp, ttime, n,
		   n_start+n, n_end); */
	}

	s_start = s_end;
	t_start = t_end;
	n_start = ntarget_pm.t(n_i);
    }

    ntarget_pm.resize(n_i,ntarget_pm.num_channels());

/*    printf("target_pm.end() = %f ntarget_pm.end() = %f\n",
      target_pm.end(), ntarget_pm.end()); */
    target_pm = ntarget_pm;
/*    printf("target_pm.end() = %f ntarget_pm.end() = %f\n",
      target_pm.end(), ntarget_pm.end()); */
    if (n_i == 0)
	map.resize(0);  // nothing to synthesize
    else
	map.resize(n_i - 1);
}

void cl_mapping(EST_Utterance &utt, LISP params)
{
    EST_Relation *target_lab;
    EST_IVector *map;
    EST_Track *source_coef=0, *target_coef=0;
    float dur_impose_factor, f0_impose_factor;

    source_coef = track(utt.relation("SourceCoef")->head()->f("coefs"));
    target_coef = track(utt.relation("TargetCoef")->head()->f("coefs"));
    target_lab = utt.relation("Segment");
    
    map = new EST_IVector;

    dur_impose_factor = get_param_float("dur_impose_factor",params,0.0);
    f0_impose_factor = get_param_float("f0_impose_factor",params,0.0);
    
    make_segment_varied_mapping(*target_lab, *source_coef,
				*target_coef, *map,
				dur_impose_factor,
				f0_impose_factor);

    utt.create_relation("US_map");
    EST_Item *item = utt.relation("US_map")->append();
    item->set_val("map", est_val(map));

}

LISP l_cl_mapping(LISP utt, LISP params)
{
    EST_Utterance *u = get_c_utt(utt);

    cl_mapping(*u,params);

    return utt;
}



