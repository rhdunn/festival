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
/*                 Acoustic Unit Concatenation                           */
/*                                                                       */
/*************************************************************************/


#include "siod.h"
#include "EST_sigpr.h"
#include "EST_wave_aux.h"
#include "EST_track_aux.h"
#include "EST_ling_class.h"
#include "us_synthesis.h"


void merge_features(EST_Item *from, EST_Item *to, int keep_id);

void dp_time_align(EST_Utterance &utt, const EST_String &source_name,
		   const EST_String &target_name, 
		   const EST_String &time_name,
		   bool do_start);

void concatenate_unit_coefs(EST_Relation &unit_stream, EST_Track &source_lpc);
void us_unit_raw_concat(EST_Utterance &utt);
void window_units(EST_Relation &unit_stream, 
		  EST_TVector<EST_Wave> &frames, 
		  float window_factor, 
		  EST_String window_name);

bool dp_match(const EST_Relation &lexical,
	      const EST_Relation &surface,
	      EST_Relation &match,
	      float ins, float del, float sub);

void map_match_times(EST_Relation &target, const EST_String &match_name,
	       const EST_String &time_name, bool do_start);
static void window_frame(EST_Wave &frame, EST_Wave &whole, float scale, 
		  int start, int end, const EST_String &window_name)
{
    int i, j, send;
    EST_TBuffer<float> window;
    
    if (frame.num_samples() != (end-start))
	frame.resize(end-start);
    frame.set_sample_rate(whole.sample_rate());
    // Ensure we have a safe end
    if (end <= whole.num_samples())
	send = end;
    else
	send = whole.num_samples();
    
    EST_Window::make_window(window, end - start, window_name);

    // To allow a_no_check access we do this is three stages
    for (i = 0, j = start; j < 0; ++i, ++j)
	frame.a_no_check(i) = 0;
    for ( ; j < send; ++i, ++j)
	frame.a_no_check(i) = (int)((float)whole.a_no_check(j) * window(i) * scale);
    for ( ; j < end; ++j,++i)
	frame.a_no_check(i) = 0;
}

void window_signal(EST_Wave &sig, EST_Track &pm, 
		   EST_WaveVector &frames, int &i, float scale, 
		   float window_factor, const EST_String &window_name)
{
    int j;    
    int first_pm, last_pm;
    float first_pos, last_pos, period2, period;

    for (j = 0; j < pm.num_frames(); ++j, ++i)
    {
	// i.e. average of local pitch period
	period = get_time_frame_size(pm, j);
	last_pos = pm.t(j) + period;
	first_pos = pm.t(j) - period;
	
	period2 = period * 2.0;
	first_pos += period2 - (period2 * window_factor);
	last_pos += (period2 * window_factor) - period2;
	
	first_pm = (int)(first_pos * (float)sig.sample_rate());
	last_pm = (int)(last_pos * (float)sig.sample_rate());
	
	if (i >= frames.length()) // exceptionally may be required
	    frames.resize((int)(frames.length()*1.2));
	window_frame(frames[i], sig, scale, first_pm, last_pm, window_name);
    }
}

void us_unit_concat(EST_Utterance &utt, float window_factor, 
		    const EST_String &window_name, bool no_waveform=false)
{
    EST_Relation *unit_stream;
    EST_WaveVector *frames = new EST_WaveVector;
    EST_Track *source_coef = new EST_Track;

    unit_stream = utt.relation("Unit", 1);

    concatenate_unit_coefs(*unit_stream, *source_coef);

    utt.create_relation("SourceCoef");
    EST_Item *item = utt.relation("SourceCoef")->append();
    item->set("name", "coef");
    item->set_val("coefs", est_val(source_coef));

    if (!no_waveform)
    {
	window_units(*unit_stream, *frames, window_factor, window_name);

//	utt.create_relation("Frames");
//	EST_Item *item = utt.relation("Frames")->append();
	item->set_val("frame", est_val(frames));
    }
}


void us_get_copy_wave(EST_Utterance &utt, EST_Wave &source_sig, 
		       EST_Track &source_coefs, EST_Relation &source_seg)
{
    EST_Item *s, *n;

    if (!utt.relation_present("Segment"))
	EST_error("utterance must have \"Segment\" relation\n"); 

    utt.create_relation("TmpSegment");

    for (s = source_seg.head(); s; s = next(s))
    {
	n = utt.relation("TmpSegment")->append();
	merge_features(n, s, 0);
    }

    utt.relation("Segment")->remove_item_feature("source_end");

    dp_time_align(utt, "TmpSegment", "Segment", "source_", 0);

    utt.create_relation("Unit");
    EST_Item *d = utt.relation("Unit")->append();


    EST_Wave *ss = new EST_Wave;
    *ss = source_sig;

    EST_Track *c = new EST_Track;
    *c = source_coefs;

    d->set_val("sig", est_val(ss));
    d->set_val("coefs", est_val(c));

    utt.remove_relation("TmpSegment");
}

void us_energy_normalise(EST_Relation &unit)
{
    EST_Wave *sig;

    for (EST_Item *s = unit.head(); s; s = next(s))
    {
	sig = wave(s->f("sig"));
	if (s->f_present("energy_factor"))
	    sig->rescale(s->F("energy_factor"));
    }
}

void us_unit_raw_concat(EST_Utterance &utt)
{
    EST_Wave *sig, *unit_sig;
    EST_Track *unit_coefs=0;
    float window_factor;
    int i, j, k;
    int first_pm, last_pm, last_length;
    float first_pos, last_pos;

    window_factor = get_c_float(siod_get_lval("window_factor",
					      "UniSyn: no window_factor"));
    sig = new EST_Wave;

    sig->resize(1000000);
    sig->fill(0);
    j = 0;

    for (EST_Item *s = utt.relation("Unit", 1)->head(); s; s = next(s))
    {
	unit_sig = wave(s->f("sig"));
	unit_coefs = track(s->f("coefs"));

	first_pos = unit_coefs->t(1);
	first_pm = (int)(first_pos * (float)unit_sig->sample_rate());

	last_pos = unit_coefs->t(unit_coefs->num_frames()-2);
	last_pm = (int)(last_pos * (float)unit_sig->sample_rate());
	last_length = unit_sig->num_samples() - last_pm;

//	cout << "first pm: " << first_pm << endl;
//	cout << "last pm: " << last_pm << endl;
//	cout << "last length: " << last_length << endl;

	j -= first_pm;

	for (i = 0; i < first_pm; ++i, ++j)
	    sig->a_safe(j) += (short)((((float) i)/ (float)first_pm) *(float)unit_sig->a_safe(i)+0.5);

	for (; i < last_pm; ++i, ++j)
	    sig->a(j) = unit_sig->a(i);

	for (k = 0; i < unit_sig->num_samples(); ++i, ++j, ++k)
	    sig->a_safe(j) += (short)((1.0 - (((float) k) / (float) last_length)) 
	      * (float)unit_sig->a_safe(i) + 0.5);

//	j -= last_length;
//	j += 2000;
    }

    sig->resize(j);
    sig->set_sample_rate(16000);

    add_wave_to_utterance(utt, *sig, "Wave");
}

void window_units(EST_Relation &unit_stream, 
		  EST_TVector<EST_Wave> &frames, 
		  float window_factor, 
		  EST_String window_name)
{
    int i;
    EST_Wave *sig;
    EST_Item *u;
    EST_Track *coefs;
    int num = 0;
    float scale;
    
    for (u = unit_stream.head(); u; u = next(u))
	num += track(u->f("coefs"))->num_frames();
    frames.resize(num);

    if (window_name == "")
	window_name = "hanning";
    
    for (i = 0, u = unit_stream.head(); u; u = next(u))
    {
	sig = wave(u->f("sig"));
	coefs = track(u->f("coefs"));
	scale = (u->f_present("scale") ? u->F("scale") : 1.0);

	window_signal(*sig, *coefs, frames, i, scale, window_factor,
		      window_name);
    }
    frames.resize(i);  // i should mostly = num, but just in case
}

void concatenate_unit_coefs(EST_Relation &unit_stream, EST_Track &source_lpc)
{
    EST_Item *u;
    int num_source_frames, num_source_channels;
    float prev_time, abs_offset, rel_offset, period, offset;
    int i, j, k, l;
    EST_Track *coefs;

    num_source_frames = num_source_channels = 0;

//    cout << "coef no ola\n";

    for (u = unit_stream.head(); u; u = next(u))
    {
	EST_Track *t = track(u->f("coefs"));
	num_source_frames += t->num_frames();
	num_source_channels = t->num_channels();
    }

    source_lpc.resize(num_source_frames, num_source_channels);
    prev_time = 0.0;

    // copy basic information
    for (i = 0, l = 0, u = unit_stream.head(); u; u = next(u))
    {
	coefs = track(u->f("coefs"));
	source_lpc.copy_setup(*coefs);

	for (j = 0; j < coefs->num_frames(); ++j, ++i)
	{
	    for (k = 0; k < coefs->num_channels(); ++k)
		source_lpc.a_no_check(i, k) = coefs->a_no_check(j, k);
	    source_lpc.t(i) = coefs->t(j) + prev_time;
	}

	prev_time = source_lpc.t(i - 1);
	u->set("end", prev_time);
    }

    // adjust pitchmarks
    abs_offset = 0.0;
    rel_offset = 0.0;
    // absolute offset in seconds
    abs_offset = get_c_float(siod_get_lval("us_abs_offset", "zz"));
    // relative offset as a function of local pitch period
    rel_offset = get_c_float(siod_get_lval("us_rel_offset", "zz"));
//    cout << "abs_offset = " << abs_offset 
//	<< " rel_offset" << rel_offset << endl;

    for (i = 0; i < source_lpc.num_frames(); ++i)
    {
	period = get_time_frame_size(source_lpc, (i));
	offset = abs_offset + (rel_offset * period);
//	cout << "rel_offset = " << rel_offset 
//	    << " abs_offset = " << abs_offset << endl;
//	cout << "offset = " << offset << endl;
	source_lpc.t(i) = source_lpc.t(i) + offset;
    }
//    cout << "source lpc size:" << source_lpc.num_frames() << endl;
}
