 /************************************************************************/
 /*                                                                      */
 /*                Centre for Speech Technology Research                 */
 /*                     University of Edinburgh, UK                      */
 /*                         Copyright (c) 1998                           */
 /*                        All Rights Reserved.                          */
 /*                                                                      */
 /*  Permission is hereby granted, free of charge, to use and distribute */
 /*  this software and its documentation without restriction, including  */
 /*  without limitation the rights to use, copy, modify, merge, publish, */
 /*  distribute, sublicense, and/or sell copies of this work, and to     */
 /*  permit persons to whom this work is furnished to do so, subject to  */
 /*  the following conditions:                                           */
 /*   1. The code must retain the above copyright notice, this list of   */
 /*      conditions and the following disclaimer.                        */
 /*   2. Any modifications must be clearly marked as such.               */
 /*   3. Original authors' names are not deleted.                        */
 /*   4. The authors' names are not used to endorse or promote products  */
 /*      derived from this software without specific prior written       */
 /*      permission.                                                     */
 /*                                                                      */
 /*  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK       */
 /*  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     */
 /*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  */
 /*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE    */
 /*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   */
 /*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  */
 /*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         */
 /*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      */
 /*  THIS SOFTWARE.                                                      */
 /*                                                                      */
 /************************************************************************/

#ifndef __CLUNITS_H__
#define __CLUNITS_H__

#include "EST_StringTrie.h"

class CLDB {
  public:
    CLDB();
    ~CLDB();

    LISP params;
    EST_StringTrie index;
    EST_StringTrie fileindex;
    EST_FVector cweights;

    EST_Item *get_unit(const EST_String &name)
	{ return (EST_Item *)index.lookup(name); }
    EST_Item *get_fileitem(const EST_String &name)
	{ return (EST_Item *)fileindex.lookup(name); }
    void load_coefs_sig(EST_Item *unit);
    void load_join_coefs(EST_Item *unit);
};

LISP cl_load_db(LISP params);
LISP acost_utt_load_coeffs(LISP utt, LISP params);
LISP make_unit_distance_tables(LISP unittypes, LISP params);
LISP ac_distance_tracks(LISP filename1, LISP filename2, LISP lweights);
void acost_dt_params(LISP params);
float ac_unit_distance(const EST_Track &unit1,
		       const EST_Track &unit2,
		       const EST_FVector wghts);
float frame_distance(const EST_Track &a, int ai,
		     const EST_Track &b, int bi,
		     const EST_FVector &wghts);
void check_cldb();
void cl_maybe_fix_pitch_c0(EST_Track *c);

extern CLDB *cldb;

#endif
