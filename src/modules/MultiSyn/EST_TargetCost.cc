/*************************************************************************/
/*                                                                       */
/*                Centre for Speech Technology Research                  */
/*                 (University of Edinburgh, UK) and                     */
/*                           Korin Richmond                              */
/*                         Copyright (c) 2002                            */
/*                         All Rights Reserved.                          */
/*                                                                       */
/*  Permission is hereby granted, free of charge, to use and distribute  */
/*  this software and its documentation without restriction, including   */
/*  without limitation the rights to use, copy, modify, merge, publish,  */
/*  distribute, sublicense, and/or sell copies of this work, and to      */
/*  permit persons to whom this work is furnished to do so, subject to   */
/*  the following conditions:                                            */
/*                                                                       */
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
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT   */
/*  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     */
/*  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   */
/*  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       */
/*  THIS SOFTWARE.                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*                        Author: Korin Richmond                         */
/*                          Date: October 2002                           */
/* --------------------------------------------------------------------- */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*************************************************************************/

#include <iostream.h>
#include "festival.h"
#include "ling_class/EST_Item.h"
#include "EST_TargetCost.h"

static const EST_String simple_pos(const EST_String s);
static const EST_Item* tc_get_syl(const EST_Item *seg);
static const EST_Item* tc_get_word(const EST_Item *seg);

/*
 *  BASE CLASS:  EST_TargetCost
 */


/* Individual cost functions */

float EST_TargetCost::stress_cost() const
{
  int cand_stress;
  int targ_stress;
  const EST_Item *tsyl, *csyl;

  if( ph_is_vowel(targ->S("name")) && 
      !ph_is_silence(targ->S("name")) )
    {
      tsyl = tc_get_syl(targ);
      csyl = tc_get_syl(cand);
      
      // Can't assume candidate and target identities are the same
      // (because of backoff to a silence for example)
      if( csyl == 0 )
	return 1;

      targ_stress = (tsyl->I("stress") > 0) ? 1 : 0;
      cand_stress = (csyl->I("stress") > 0) ? 1 : 0;

      if( cand_stress != targ_stress)
	return 1;
    }
  
  if( ph_is_vowel(targ->next()->S("name")) &&
      !ph_is_silence(targ->next()->S("name")) )
    {
      tsyl = tc_get_syl(targ->next());
      csyl = tc_get_syl(cand->next());

      // Can't assume candidate and target identities are the same
      // (because of backoff to a silence for example)
      if( csyl == 0 )
	return 1;

      targ_stress = (tsyl->I("stress") > 0) ? 1 : 0;
      cand_stress = (csyl->I("stress") > 0) ? 1 : 0;
      if( cand_stress != targ_stress)
 	return 1;
    }
  
  return 0;
}

float EST_TargetCost::position_in_syllable_cost() const
{
   tcpos_t targ_pos = TCPOS_MEDIAL;
   tcpos_t cand_pos = TCPOS_MEDIAL;

   const EST_Item *targ_syl = tc_get_syl(targ);
   const EST_Item *targ_next_syl = tc_get_syl(targ->next());
   const EST_Item *targ_next_next_syl = tc_get_syl(targ->next()->next());
   const EST_Item *targ_prev_syl = tc_get_syl(targ->prev());
   const EST_Item *cand_syl = tc_get_syl(cand);
   const EST_Item *cand_next_syl = tc_get_syl(cand->next());
   const EST_Item *cand_next_next_syl = tc_get_syl(cand->next()->next());
   const EST_Item *cand_prev_syl = tc_get_syl(cand->prev());
   
   if( targ_syl != targ_next_syl )
     targ_pos = TCPOS_INTER;
   else if( targ_syl != targ_prev_syl)
     targ_pos = TCPOS_INITIAL;
   else if( targ_next_syl != targ_next_next_syl)
     targ_pos = TCPOS_FINAL;
   
   if( cand_syl != cand_next_syl )
     cand_pos = TCPOS_INTER;
   else if( cand_syl != cand_prev_syl)
     cand_pos = TCPOS_INITIAL;
   else if( cand_next_syl != cand_next_next_syl)
     cand_pos = TCPOS_FINAL;
   
   return (targ_pos == cand_pos) ? 0 : 1;
}

float EST_TargetCost::position_in_word_cost() const
{
  tcpos_t targ_pos = TCPOS_MEDIAL;
  tcpos_t cand_pos = TCPOS_MEDIAL;
  
  const EST_Item *targ_word = tc_get_word(targ);
  const EST_Item *targ_next_word = tc_get_word(targ->next());
  const EST_Item *targ_next_next_word = tc_get_word(targ->next()->next());
  const EST_Item *targ_prev_word = tc_get_word(targ->prev());
  const EST_Item *cand_word = tc_get_word(cand);
  const EST_Item *cand_next_word = tc_get_word(cand->next());
  const EST_Item *cand_next_next_word = tc_get_word(cand->next()->next());
  const EST_Item *cand_prev_word = tc_get_word(cand->prev());
  
  if( targ_word != targ_next_word )
    targ_pos = TCPOS_INTER;
  else if( targ_word != targ_prev_word)
    targ_pos = TCPOS_INITIAL;
  else if( targ_next_word != targ_next_next_word)
    targ_pos = TCPOS_FINAL;
  
  if( cand_word != cand_next_word )
    cand_pos = TCPOS_INTER;
  else if( cand_word != cand_prev_word)
    cand_pos = TCPOS_INITIAL;
  else if( cand_next_word != cand_next_next_word)
     cand_pos = TCPOS_FINAL;
  
  return (targ_pos == cand_pos) ? 0 : 1;
}


float EST_TargetCost::position_in_phrase_cost() const
{
  
  const EST_Item *targ_word = tc_get_word(targ);
  const EST_Item *cand_word = tc_get_word(cand);
  
  if (!targ_word && !cand_word)
    return 0;
  if (!targ_word || !cand_word)
    return 1;

  return (targ_word->S("pbreak") == cand_word->S("pbreak")) ? 0 : 1;
}


float EST_TargetCost::partofspeech_cost() const
{
  // Compare left phone half of diphone
  const EST_Item *targ_left_word = tc_get_word(targ);
  const EST_Item *cand_left_word = tc_get_word(cand);
  
  if(!targ_left_word && !cand_left_word)
    return 0;
  if(!targ_left_word || !cand_left_word)
    return 1;

  const EST_String targ_left_pos  = simple_pos(targ_left_word->S("pos"));
  const EST_String cand_left_pos  = simple_pos(cand_left_word->S("pos"));

  if( targ_left_pos != cand_left_pos )
    return 1;

  // Compare right phone half of diphone
  const EST_Item *targ_right_word = tc_get_word(targ->next());
  const EST_Item *cand_right_word = tc_get_word(cand->next());

  if(!targ_right_word && !cand_right_word)
    return 0;
  if(!targ_right_word || !cand_right_word)
    return 1;

  const EST_String targ_right_pos = simple_pos(targ_right_word->S("pos"));
  const EST_String cand_right_pos = simple_pos(cand_right_word->S("pos"));

  if( targ_right_pos != cand_right_pos )
    return 1;

  return 0;
}

float EST_TargetCost::left_context_cost() const
{
  
  EST_Item *targ_context = targ->prev();
  EST_Item *cand_context = cand->prev();
  
  if ( !targ_context && !cand_context)
     return 0;
  if ( !targ_context  || !cand_context)
    return 1;

  return (targ_context->S("name") == cand_context->S("name")) ? 0 : 1;
}

float EST_TargetCost::right_context_cost() const
{
  
  EST_Item *targ_context = targ->next()->next();
  EST_Item *cand_context = cand->next()->next();
  
  if ( !targ_context && !cand_context)
    return 0;
  if ( !targ_context  || !cand_context)
    return 1;
  
  return (targ_context->S("name") == cand_context->S("name")) ? 0 : 1;
}

float EST_TargetCost::bad_duration_cost() const
{
  static const EST_String bad_dur_feat("bad_dur");
  
  // bad_dur may at some stage be set on a target for resynthesis purposes.
  if( cand->f_present(bad_dur_feat) 
      != targ->f_present(bad_dur_feat) )
    return 1.0;
  
  if( cand->next()->f_present(bad_dur_feat) 
      != targ->next()->f_present(bad_dur_feat) )
    return 1.0;
  
  return 0.0;
}

//////////////////////////////////////////////////////////////////////
// (Should be fixed not to have hardcoded FVector indicies)
float EST_TargetCost::bad_f0_cost() const
{
  const EST_Item *cand_left = cand;
  const EST_Item *cand_right = cand_left->next();

  EST_String left_phone(  cand_left->S("name")  );
  EST_String right_phone( cand_right->S("name") );  

  EST_FVector *fv = 0;
  float penalty = 0.0;

  if( ph_is_vowel( left_phone )
      || ph_is_approximant( left_phone )
      || ph_is_liquid( left_phone )
      || ph_is_nasal( left_phone ) ){
    fv = fvector( cand_left->f("midcoef") );
    if( (*fv)[13] == -1.0 ) // means unvoiced
      penalty += 0.5;
  }
  
  if( ph_is_vowel( right_phone )
      || ph_is_approximant( right_phone )
      || ph_is_liquid( right_phone )
      || ph_is_nasal( right_phone ) ){
    fv = fvector( cand_right->f("midcoef") );
    if( (*fv)[13] == -1.0 ) // means unvoiced 
      penalty += 0.5;
  }

  return penalty; 
}


/*
 *  DERIVED CLASS: EST_DefaultTargetCost
 *
 *  This is CSTR's proposed default target cost. Nothing special, if you think you can
 *  do better derive your own class.
 */

float EST_DefaultTargetCost::operator()(const EST_Item* targ, const EST_Item* cand) const 
{ 
  set_targ_and_cand(targ,cand);
  score = 0.0;
  weight_sum = 0.0;

  score += add_weight(10.0)*stress_cost();
  score += add_weight(5.0)*position_in_syllable_cost();
  score += add_weight(5.0)*position_in_word_cost();
  score += add_weight(6.0)*partofspeech_cost();
  score += add_weight(7.0)*position_in_phrase_cost();
  score += add_weight(4.0)*left_context_cost();
  score += add_weight(3.0)*right_context_cost();
  score += add_weight(10.0)*bad_duration_cost();
  score += add_weight(25.0)*bad_f0_cost();

  return score / weight_sum;
}

/*
 *  DERIVED CLASS: EST_SchemeTargetCost
 *
 *  This lets you implement your target cost in scheme, so you can
 *  change it on the fly. Great for developement, but about 5 times as slow.
 *  
 */


float EST_SchemeTargetCost::operator()( const EST_Item* targ, const EST_Item* cand ) const
 { 
   LISP r,l;

   l = cons(tc,
 	   cons( siod(targ), cons( siod(cand), NIL) ));
   r = leval(l,NIL);
   if ((consp(r)) || (r == NIL) || !(numberp(r)))
     {
       cerr << "Lisp function: " << tc << 
 	" did not return float score" << endl;
       festival_error();
     }
   else
     score = get_c_float(r);
  
   return score;  
 }



/* 
 *   Auxillary target cost functions
 */


static const EST_String simple_pos(const EST_String s)
{
  if( s == "nn" || s == "nnp" || s == "nns" || s == "nnps" || s == "fw" || s == "sym" || s == "ls")
    return "n";
  if( s == "vbd" || s == "vb" || s == "vbn" || s == "vbz" || s == "vbp" || s == "vbg")
    return "v";
  if( s == "jj" || s == "jjr" || s == "jjs" || s == "1" || s == "2" || s == "rb" || 
      s == "rp" || s == "rbr" || s == "rbs")
    return "other";
  return "func";
}

static const EST_Item *tc_get_syl(const EST_Item *seg)
{
  //  if(!seg)
  //  return 0;
  
  return parent(seg,"SylStructure");
}

static const EST_Item *tc_get_word(const EST_Item *seg)
 {
   // if(!seg)
   //  return 0;
   const EST_Item *syl = tc_get_syl(seg);
   
   if(syl)
     return parent(syl,"SylStructure");
   else
     return 0;
 }
