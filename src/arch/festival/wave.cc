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
/*                     Author :  Alan W Black                            */
/*                     Date   :  October 1996                            */
/*-----------------------------------------------------------------------*/
/*                                                                       */
/*   Interface to various low level waveform functions from Lisp         */
/*                                                                       */
/*=======================================================================*/
#include <stdio.h>
#include "EST_unix.h"
#include <stdlib.h>
#include "festival.h"
#include "festivalP.h"

static void utt_save_f0_from_targets(EST_Utterance *u,EST_String &filename);
static float f0_interpolate(EST_Item *ptval, EST_Item *tval, float time);

EST_Wave *get_utt_wave(EST_Utterance *u)
{
    EST_Relation *r;

    if (((r = u->relation("Wave")) == 0) || (r->head() == 0))
    {
	cerr << "no waveform in utterance" << endl;
	festival_error();
    }

    return wave(r->head()->f("wave"));
}

static LISP wave_save(LISP lwave,LISP fname,LISP ftype,LISP stype)
{
    EST_Wave *w = wave(lwave);
    EST_String filename,filetype,sampletype;

    if (fname == NIL)
	filename = "save.wav";
    else
	filename = get_c_string(fname);
    if (ftype == NIL)
    {
	if (ft_get_param("Wavefiletype"))
	    filetype = get_c_string(ft_get_param("Wavefiletype"));
	else
	    filetype = "nist";
    }
    else
	filetype = get_c_string(ftype);
    if (stype == NIL)
    {
	if (ft_get_param("Wavesampletype"))
	    sampletype = get_c_string(ft_get_param("Wavesampletype"));
	else
	    sampletype = "short";
    }
    else
	sampletype = get_c_string(stype);
    
    if (w->save_file(filename,filetype,sampletype,EST_NATIVE_BO) != write_ok)
    {
	cerr << "utt.save.wave: failed to write wave to \"" << filename 
	    << "\"" << endl;
	festival_error();
    }
    
    return truth;
}

static LISP wave_load(LISP fname,LISP ftype,LISP stype,LISP srate)
{
    EST_Wave *w = new EST_Wave;
    EST_read_status r;

    if (ftype == NIL)
	r = w->load(get_c_string(fname));
    else if (streq("raw",get_c_string(ftype)))
	r = w->load_file(get_c_string(fname),
			 get_c_string(ftype),
			 get_c_int(srate),
			 get_c_string(stype),
			 EST_NATIVE_BO,
			 1);
    else
	r = w->load(get_c_string(fname),get_c_string(ftype));

    if (r != format_ok)
	cerr << "Cannot load wavefile: " << get_c_string(fname) << endl;
    
    return siod(w);
}

static LISP wave_copy(LISP w)
{
    return siod(new EST_Wave(*wave(w)));
}

static LISP wave_append(LISP w1,LISP w2)
{
    EST_Wave *wave1 = wave(w1);
    EST_Wave *wave2 = wave(w2);

    *wave1 += *wave2;

    return w1;
}

static LISP wave_info(LISP w1)
{
    EST_Wave *w = wave(w1);

    return cons(make_param_float("num_samples",
				w->num_samples()),
		cons(make_param_float("sample_rate",
				     w->sample_rate()),
		     cons(make_param_float("num_channels",
					  w->num_channels()),
			  cons(make_param_str("file_type",
					      w->file_type()),
			       NIL))));
}

static LISP wave_resample(LISP w1,LISP newrate)
{
    EST_Wave *w = wave(w1);
    
    w->resample(get_c_int(newrate));

    return w1;
}

static LISP wave_rescale(LISP lw,LISP lgain,LISP normalize)
{
    EST_Wave *w = wave(lw);
    float gain = get_c_float(lgain);
    
    if (normalize)
	w->rescale(gain,TRUE);
    else
	w->rescale(gain);

    return lw;
}

void play_wave(EST_Wave *w)
{
    EST_Option al;
    LISP audio;
	
    if (audsp_mode)  // asynchronous mode
	audsp_play_wave(w);
    else
    {
	if ((audio = ft_get_param("Audio_Method")) != NIL)
	    al.add_item("-p",get_c_string(audio));
	if ((audio = ft_get_param("Audio_Device")) != NIL)
	    al.add_item("-audiodevice",get_c_string(audio));
	if ((audio = ft_get_param("Audio_Command")) != NIL)
	    al.add_item("-command",quote_string(get_c_string(audio)));
	if ((audio = ft_get_param("Audio_Required_Rate")) != NIL)
	    al.add_item("-rate",get_c_string(audio));
	if ((audio = ft_get_param("Audio_Required_Format")) != NIL)
	    al.add_item("-otype",get_c_string(audio));
	al.add_item("-quality","HIGH");
	play_wave(*w,al);
    }
}

static LISP wave_play(LISP lw)
{
    play_wave(wave(lw));
    return truth;
}

static LISP track_save(LISP ltrack,LISP fname,LISP ftype)
{
    EST_Track *t = track(ltrack);
    EST_String filename,filetype;

    filename = (fname == NIL) ? "save.track" : get_c_string(fname);
    filetype = (ftype == NIL) ? "save.track" : get_c_string(ftype);
    
    if (t->save(filename, filetype) != write_ok)
    {
	cerr << "track.save: failed to write track to \"" << filename 
	    << "\"" << endl;
	festival_error();
    }
    
    return truth;
}

static LISP track_load(LISP fname,LISP ftype,LISP ishift)
{
    EST_Track *t = new EST_Track;
    EST_read_status r;
    float is = 0.0;
    if (ishift)
	is = get_c_float(ishift);

    if (ftype == NIL)
	r = t->load(get_c_string(fname),is);
    else 
	r = t->load(get_c_string(fname),
		    get_c_string(ftype),
		    is);

    if (r != format_ok)
	cerr << "Cannot load track: " << get_c_string(fname) << endl;
    
    return siod(t);
}

static LISP track_copy(LISP t)
{
    return siod(new EST_Track(*track(t)));
}

static LISP utt_save_f0(LISP utt, LISP fname)
{
    // Save utt's F0 in fname as an ESPS file
    EST_Utterance *u = utterance(utt);
    EST_String filename = get_c_string(fname);

    if ((u->relation_present("F0")) && (u->relation("F0")->head() != 0))
    {
	EST_Track *f0 = track(u->relation("F0")->head()->f("f0"));
	if (f0->save(filename,"esps") != write_ok)
	{
	    cerr << "utt.save.f0: failed to write f0 to \"" << 
		filename << "\"" << endl;
	    festival_error();
	}
    }
    else if (u->relation("Target") != 0)
	utt_save_f0_from_targets(u,filename);
    else
    {
	cerr << "utt.save.f0: utterance doesn't contain F0 or Target stream"
	    << endl;
	festival_error();
    }
    return utt;
}

static void utt_save_f0_from_targets(EST_Utterance *u,EST_String &filename)
{
    // Modifications by Gregor Moehler to do proper target tracing (GM)
    EST_Item *s;
    EST_Track f0;
    float p = 0.0;
    float length = u->relation("Segment")->last()->f("end");
    int i,frames = (int)(length / 0.010);
    f0.resize(frames,4);
    
    EST_Item *ptval, *tval;

    ptval = tval = u->relation("Target")->first_leaf();
    for (i=0,s=u->relation("Segment")->first(); s != 0; s=next(s))
    {
	if (i >= frames)
	    break;  // may hit here one before end
	if (tval != 0 && p > (float)ffeature(tval,"pos"))
	{
	    ptval = tval;
	    tval = next_leaf(tval);
	}
	for ( ; p < s->F("end",0); p+=0.010,i++)
	{
	    if (i >= frames)
		break;  // may hit here one before end
	    if ((ffeature(s,"ph_vc") == "+") ||
		(ffeature(s,"ph_cvox") == "+"))
	    {
		f0(i,0) = f0_interpolate(ptval,tval,p);
		f0(i,1) = 1;
	    }
	    else
	    {
		f0(i,0) = 0;
		f0(i,1) = 0.0; // unvoiced;
	    }
	}
    }
    f0.set_channel_name("F0",0);
    f0.set_channel_name("prob_voice",1);
    f0.fill_time(0.01);

    if (f0.save(filename,"esps") != write_ok)
    {
	cerr << "utt.save.f0: failed to write F0 to \"" << 
	    filename << "\"" << endl;
	festival_error();
    }

    return;
}

static float f0_interpolate(EST_Item *ptval, EST_Item *tval, float time)
{
    // GM: changed, to use proper targets
    // Return interpolated F0 at time t
    float p1,p0,d1,d0;

    d0=0;
    d1=0;

    if (tval == 0)                 // after last target
 	return ffeature(ptval,"f0");
    
    else if (time < (float) ffeature(ptval,"pos"))  // before 1st target
 	return ffeature(tval,"f0");

    else {
 	p0 = ffeature(ptval,"f0");
	p1 = ffeature(tval,"f0");
	d0 = ffeature(ptval,"pos");
	d1 = ffeature(tval,"pos");
    }    
    
    if (p0 == 0.0 || d1 == d0)
	return  p1;       
    else if (p1 == 0.0)
	return p0;        
    else
	return p0 + (p1-p0)*(time-d0)/(d1-d0);
}

static LISP utt_send_wave_client(LISP utt)
{
    // Send the waveform to a client (must be acting as server)
    EST_Utterance *u = utterance(utt);
    EST_Wave *w;
    EST_String tmpfile = make_tmp_filename();
    LISP ltype;
    EST_String type;

    w = get_utt_wave(u);
    if (ft_server_socket == -1)
    {
	cerr << "utt_send_wave_client: not in server mode" << endl;
	festival_error();
    }
	
    ltype = ft_get_param("Wavefiletype");
    if (ltype == NIL)
	type = "nist";
    else
	type = get_c_string(ltype);
    w->save(tmpfile,type);
    write(ft_server_socket,"WV\n",3);
    socket_send_file(ft_server_socket,tmpfile);
    unlink(tmpfile);

    return utt;
}

static LISP send_sexpr_to_client(LISP l)
{
    EST_String sl;
    unsigned int i,n,csllen;

    sl = siod_sprint(l);
    const char *csl = sl;
    csllen = strlen(csl);

    write(ft_server_socket,"LP\n",3);
    for (i=0; i < csllen; i+=n)
    {
	n = write(ft_server_socket,csl,( i+256 < csllen ? 256 : csllen-i));
    }

    return l;
}

void festival_wave_init(void)
{
    // declare utterance (wave) specific Lisp functions 

    init_subr_4("wave.save",wave_save,
 "(wave.save WAVE FILENAME FILETYPE SAMPLETYPE)\n\
  Save WAVE in FILENAME, respecting FILETYPE and SAMPLETYPE if specifed\n\
  if these last two arguments are unspecified the global parameters\n\
  Wavefiletype and Wavesampletype are used.  Returns t is successful\n\
  and throws an error if not.");  
    init_subr_4("wave.load",wave_load,
 "(wave.load FILENAME FILETYPE SAMPLETYPE SAMPLERATE)\n\
 Load and return a wave from FILENAME.  Respect FILETYPE is specified\n\
 if not specified respect whatever header is on the file.  SAMPLETYPE\n\
 and SAMPLERATE are only used if FILETYPE is raw.");
    init_subr_1("wave.copy",wave_copy,
  "(wave.copy WAVE)\n\
  Return a copy of WAVE.");
    init_subr_2("wave.append",wave_append,
  "(wave.copy WAVE1 WAVE2)\n\
  Destuctively append WAVE2 to WAVE1 and return WAVE1.");
    init_subr_1("wave.info",wave_info,
  "(wave.info WAVE)\n\
  Returns assoc list of info about this wave.");
    init_subr_2("wave.resample",wave_resample,
  "(wave.resample WAVE NEWRATE)\n\
  Resamples WAVE to NEWRATE.");
    init_subr_3("wave.rescale",wave_rescale,
  "(wave.rescale WAVE GAIN NORMALIZE)\n\
  If NORMALIZE is specified and non-nil, maximizes the waveform first\n\
  before applying the gain.");
    init_subr_1("wave.play",wave_play,
  "(wave.play WAVE)\n\
  Play wave of selected audio");

    init_subr_3("track.save",track_save,
 "(track.save TRACK FILENAME FILETYPE)\n\
  Save TRACK in FILENAME, in formar FILETYPE, est is used if FILETYPE\n\
  is unspecified or nil.");
    init_subr_3("track.load",track_load,
 "(track.load FILENAME FILETYPE ISHIFT)\n\
 Load and return a track from FILENAME.  Respect FILETYPE is specified\n\
 and ISHIFT if specified.");
    init_subr_1("track.copy",track_copy,
  "(track.copy TRACK)\n\
  Return a copy of TRACK.");

    init_subr_1("utt.send.wave.client",utt_send_wave_client,
 "(utt.send.wave.client UTT)\n\
  Sends wave in UTT to client.  If not in server mode gives an error\n\
  Note the client must be expecting to receive the waveform.");
    init_subr_1("send_sexpr_to_client", send_sexpr_to_client,
 "(send_sexpr_to_client SEXPR)\n\
Sends given sexpression to currently connected client.");
    init_subr_2("utt.save.f0",utt_save_f0,
 "(utt.save.f0 UTT FILENAME)\n\
 Save F0 of UTT as esps track file in FILENAME.");

}

    

