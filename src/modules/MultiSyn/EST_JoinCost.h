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
/*                          Author: Korin Richmond                       */
/*                            Date: October 2002                         */
/* --------------------------------------------------------------------- */
/* Interface for family of join cost function objects which              */
/* calculate a join score for given candidates                           */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


#ifndef __EST_JOINCOST_H__
#define __EST_JOINCOST_H__

class EST_Item;
class EST_JoinCostCache;

///////////////////////////////
//because of the inline 
#include "EST_JoinCostCache.h"
#include "safety.h"
#include "ling_class/EST_Item.h"
#include "EST_FMatrix.h"
///////////////////////////////

/**@name Interface for Join Cost function object
*/ 

//@{

/** Object oriented approach for better or for worse... 
*/

class EST_JoinCost {
 public:
  
  EST_JoinCost() 
    : defCost(1),
    cachedItem(0),
    cachedItemVector(0)
    {};
  
  ~EST_JoinCost();

  bool computeAndCache( const EST_TList<EST_Item*> &list, bool verbose=true );
  
  inline float operator()( const EST_Item* left, const EST_Item* right ) const;

 private: 
  float defCost;
  mutable const EST_Item *cachedItem;
  mutable const EST_FVector *cachedItemVector;
  mutable unsigned int cached_jccid;
  mutable unsigned int cached_jccindex;
  mutable bool costIsCached;

  EST_TSimpleVector<EST_JoinCostCache *> costCaches;
};


//////////////////////////////////////////////////////////////////////////////////
// experiment to see if this is sensible or not

// // for now, the left and right edges of a join are represented by EST_Items,
// // which is particular to the implementation of the DiphoneUnitVoice.
// // It would be desirable in the future though to abstract the interface to 
// // something like concatUnit::right_edge() and concatUnit::left_edge() since
// // we would like a given join cost to be able to be generally applied to units of
// // any length or form.

inline float EST_JoinCost::operator()( const EST_Item* left, const EST_Item* right ) const
{
  float d_spectral, d_f0, d_power, d_overall;
  
  //to avoid the overhead of creating and destroying these
  static const EST_String jccid_str( "jccid" );
  static const EST_String jccindex_str( "jccindex" );
  static const EST_String midcoef_str( "midcoef" );
  
  if( left == right->prev() )
    //default zero cost if units contiguous in database
    // (i.e. this is the cost between a phone and *itself*)
    d_overall=0.0;     
  else{

    // since the Viterbi class takes each path at time t and tries to extend
    // with all candidates at time t+1, it probably makes sense to cache
    // the feature vector for the "left" item to avoid looking it up every time
    // this function is called...
    if( cachedItem != left ){
      cachedItem = left;
      
      if( left->f_present( jccid_str ) ){
	costIsCached = true;
	cached_jccid = left->I( jccid_str );
	cached_jccindex = left->I( jccindex_str );
      }
      else{
	costIsCached = false;
	cachedItemVector = fvector( left->f( midcoef_str ) );
      }
    }
    
    if( costIsCached && right->f_present( jccid_str ) ){
      unsigned int right_jccid = right->I( jccid_str );
      unsigned int right_jccindex = right->I( jccindex_str );
      
      if( cached_jccid == right_jccid )
	d_overall = (float)(costCaches(cached_jccid)->val(cached_jccindex, right_jccindex))/255;
      else{
	EST_warning( "JoinCost: inconsistent cache ids, setting max join cost" );
	d_overall = 1.0;
      }
    }
    else{
      const EST_FVector *l = cachedItemVector;
      const EST_FVector *r = fvector( right->f( midcoef_str ) );
      
      int l_length = l->length();
      if (l_length != r->length())
	EST_error("Can't compare vectors of differing length\n");
      
      ////////////////////////////////////////////////////////////////////////////
      // f0 distance
      //
      // because unvoiced is represented as -1.0, we need to take special measures
      // when calculating f0 distances, to avoid situation where something low in
      // the speaker's range is closer to 0.0 than other higher voiced speech.
      // (this could especially be problematic where bad pitchmarking or labelling
      // occurs)
      float l_f0 = l->a_no_check( l_length-1 );
      float r_f0 = r->a_no_check( l_length-1 );
      
      if( l_f0 != -1.0 ){
	if( r_f0 != -1.0 ){
	  d_f0 = pow(( l_f0 - r_f0 ), 2.0);
	  d_f0 = sqrt( d_f0 );
	}
	else
	  d_f0 = 1.0;
      }
      else if( r_f0 != -1.0 )
	d_f0 = 1.0;
      else
	d_f0 = 0.0;
      
      // power distance
      d_power = pow((l->a_no_check(l_length-2) - r->a_no_check(l_length-2)), 2.0);
      d_power = sqrt( d_power );
      
      // spectral distance
      float d = 0.0;
      l_length -= 2; // don't include f0 and power
      for(int i = 0; i < l_length; i++)
	d += pow((l->a_no_check(i) - r->a_no_check(i)), 2.0);
      d_spectral = sqrt( d );
      
      // equal weighting (should allow setting weights at runtime in future)
      d_overall = (d_f0 + d_power + d_spectral)/3.0;
    }
}
  return d_overall; 
}



#endif // __EST_JOINCOST_H__





