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
/*                            Date:  Aug  2002                           */
/* --------------------------------------------------------------------- */
/* first stab at a diphone unit selection "voice" - using a list of      */
/* utterance objects                                                     */
/*************************************************************************/

#include "DiphoneUnitVoice.h"
#include "DiphoneVoiceModule.h"
#include "EST_rw_status.h"
#include "EST_viterbi.h"
#include "EST_Track.h"
#include "EST_track_aux.h"
#include "EST_Wave.h"
#include "EST_THash.h"
#include "EST_TList.h"
#include "EST_types.h"
#include "ling_class/EST_Utterance.h"
#include "siod.h"
#include "siod_est.h"
#include "safety.h"
#include <stdlib.h>

#include "EST_TargetCost.h"
#include "EST_JoinCost.h"
#include "EST_JoinCostCache.h"

#include "EST_Val.h"


// from src/modules/UniSyn_diphone/us_diphone.h
// this won't be staying here long...
void parse_diphone_times(EST_Relation &diphone_stream, 
			 EST_Relation &source_lab);

SIOD_REGISTER_CLASS(du_voice,DiphoneUnitVoice)
VAL_REGISTER_CLASS(du_voice,DiphoneUnitVoice)


// temporary hack necessary because decoder can only take a 
// function pointer (would be better to relax this restriction in 
// the EST_Viterbi_Decoder class, or in a replacement class, rather
// than using this hack)
static DiphoneUnitVoice *globalTempVoicePtr = 0;

DiphoneUnitVoice::DiphoneUnitVoice( const EST_StrList& basenames,
				    const EST_String& uttDir,
				    const EST_String& wavDir,
				    const EST_String& pmDir,
				    const EST_String& coefDir,
				    unsigned int sr,
				    const EST_String& uttExt,
				    const EST_String& wavExt,
				    const EST_String& pmExt,
				    const EST_String& coefExt )
  : pruning_beam( -1 ),
    ob_pruning_beam( -1 ),
    wav_srate( sr ),
    jc( 0 ),
    jc_delete( false ),
    tc( 0 ),
    tc_delete( false )

{
  // make the default voice module with the supplied parameters
  addVoiceModule( basenames, uttDir, wavDir, pmDir, coefDir,
		  wav_srate,
		  uttExt, wavExt, pmExt, coefExt );

  diphone_backoff_rules = 0;
}

void DiphoneUnitVoice::initialise()
{
  if( jc == 0 )
    EST_error( "Need to set join cost calculator for voice" );

  if( tc == 0 )
    EST_error( "Need to set target cost calculator for voice" );

  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin(voiceModules); it; it++ )
    (*it)->initialise();
}

bool DiphoneUnitVoice::addVoiceModule( const EST_StrList& basenames,
				       const EST_String& uttDir,
				       const EST_String& wavDir,
				       const EST_String& pmDir,
				       const EST_String& coefDir,
				       unsigned int srate,
				       const EST_String& uttExt,
				       const EST_String& wavExt,
				       const EST_String& pmExt,
				       const EST_String& coefExt )

{
  DiphoneVoiceModule *vm;

  if( srate != wav_srate )
    EST_error( "Voice samplerate: %d\nmodule samplerate: %d", 
	       wav_srate, srate );

  vm = new DiphoneVoiceModule( basenames, uttDir, wavDir, pmDir, coefDir,
			       srate,
			       uttExt, wavExt, pmExt, coefExt );
  CHECK_PTR(vm);
  
  registerVoiceModule( vm );

  return true;
}
  

void DiphoneUnitVoice::registerVoiceModule( DiphoneVoiceModule *vm )
{
  voiceModules.append( vm );
}


void DiphoneUnitVoice::setJoinCost( EST_JoinCost *jcost, bool del )
{
  if( jc_delete == true )
    if( jc != 0 )
      delete jc;

  jc = jcost;
  jc_delete = del;
}

void DiphoneUnitVoice::setTargetCost( EST_TargetCost *tcost, bool del )
{
  if( tc_delete == true )
    if( tc != 0 )
      delete tc;

  tc = tcost;
  tc_delete = del;
}



DiphoneUnitVoice::~DiphoneUnitVoice()
{
  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin(voiceModules); it; it++ )
    delete( *it );

  if(diphone_backoff_rules)
    delete diphone_backoff_rules;

  if( jc_delete == true )
    if( jc != 0 )
      delete jc;

  if( tc_delete == true )
    if( tc != 0 )
      delete tc;
}


void DiphoneUnitVoice::addToCatalogue( const EST_Utterance *utt )
{
  // needed?
}


void DiphoneUnitVoice::getDiphone( const EST_VTCandidate *cand, 
				   EST_Track* coef, EST_Wave* sig, int *midframe )
{
  // The need for this function in this class is a bit messy, it would be far
  // nicer just to be able to ask the Candidate itself to hand over the relevant
  // synthesis parameters.  In future, it will work that way ;)

  // put there by DiphoneVoiceModule::getCandidateList
  const DiphoneVoiceModule* parentModule = (diphonevoicemoduleptr( cand->name ))->ptr;  
  EST_Item *firstPhoneInDiphone = cand->s;

  // need to call right getDiphone to do the actual work
  parentModule->getDiphone( firstPhoneInDiphone, coef, sig, midframe );
} 



// The use of the globalFunctionPtr in this function is a really just a temporary hack 
// necessary because the decoder as it stands at present can only take a function pointer 
// (would be better to relax this restriction in the EST_Viterbi_Decoder class, or in a 
// replacement class, rather than using this hack)
static EST_VTPath* extendPath( EST_VTPath *p, EST_VTCandidate *c,
	 		       EST_Features&)
{ 
  EST_VTPath *np = new EST_VTPath;
  CHECK_PTR(np);

  if( globalTempVoicePtr ==0 )
    EST_error( "globalTempVoicePtr is not set, can't continue" );
  
  const EST_JoinCost &jcost = globalTempVoicePtr->getJoinCostCalculator();
  
  np->c = c;
  np->from = p;
  np->state = c->pos;
  
  if ((p == 0) || (p->c == 0))
    np->score = c->score;
  else{
    // join cost between right edge of left diphone and vice versa
    np->score = p->score + c->score + jcost( p->c->s->next(), c->s );
  }
  return np;
}

// This function is a really just a temporary hack necessary because the decoder
// as it stands at present can only take a function pointer (would be better to relax
// this restriction in the EST_Viterbi_Decoder class, or in a replacement class, rather
// than using this hack)
static EST_VTCandidate* getCandidatesFunction( EST_Item *s, EST_Features &f )
{
  DiphoneUnitVoice *duv = globalTempVoicePtr;  
  if( duv==0 )
    EST_error( "Candidate source voice is unset" );

  return duv->getCandidates( s, f );
}

// Function which, given an item from the timeline relation that
// was originally used to initialise the EST_Viterbi_Decoder
// returns a pointer to a linked list of EST_VTCandidates
// (this is provided to the viterbi decoder upon its construction
// and (in)directly called by it as part of the decoding process...)
EST_VTCandidate* DiphoneUnitVoice::getCandidates( EST_Item *s, EST_Features &f ) const
{
  EST_VTCandidate *c = 0;
  EST_VTCandidate *moduleListHead = 0;
  EST_VTCandidate *moduleListTail = 0;

  // these objects [c/sh]ould be a parameter visible in the user's script
  // land, and will be in future...

  // tc now a member
  // EST_DefaultTargetCost default_target_cost;
  // EST_TargetCost *tc = &default_target_cost;
  // or
  //  EST_SchemeTargetCost scheme_target_cost(rintern( "targetcost"));
  //  EST_TargetCost *tc = &scheme_target_cost;

  EST_TList<DiphoneVoiceModule*>::Entries module_iter;
  int nfound, total=0;

  ////////////////////////////////////////////////////////////////
  // join linked list of candidates from each module into one list
  for( module_iter.begin(voiceModules); module_iter; module_iter++ ){
    nfound = (*module_iter)->getCandidateList( *s, 
					       *tc, 
					       &moduleListHead, 
					       &moduleListTail );
    if( nfound>0 ){
      moduleListTail->next = c;
      c = moduleListHead;
      total += nfound;
    }
  }

  if( total==0 )
    EST_error( "Couldn't find diphone %s", (const char*)s->S("name") );
  
  if( verbosity() > 0 )
    printf( "Number of candidates found for target: %d\n", total );

  return c;
}

bool DiphoneUnitVoice::synthesiseWave( EST_Utterance *utt )
{
  getUnitSequence( utt );

  return true; 
}



void DiphoneUnitVoice::getUnitSequence( EST_Utterance  *utt )
{
  EST_Relation *segs = utt->relation( "Segment" );
  EST_Relation *units = utt->create_relation( "Unit" );
  
  // Initialise the Unit relation time index for decoder
  EST_String diphone_name;
  EST_StrList missing_diphones;

  EST_Item *it=segs->head();
  if( it == 0 )
    EST_error( "Segment relation is empty" );
  
  for( ; it->next(); it=it->next() )
    {
      EST_String l = it->S("name");
      EST_String r = it->next()->S("name");
      
      EST_String diphone_name = EST_String::cat(l,"_",r);
      
      // Simple back off.
      // Change diphone name for one we actually have.
           
      while(diphone_name != EST_String::Empty &&
	    !this->unitAvailable(diphone_name) && 
	    diphone_backoff_rules)
	{
	  EST_String orig = diphone_name;
	  
	  diphone_name = diphone_backoff_rules->backoff(l,r);
	  l = diphone_name.before("_");
	  r = diphone_name.after("_");
	  
	  if( verbosity() > 0 ){
	    EST_warning("Backing off requested diphone %s to %s", 
			orig.str(), 
			diphone_name.str() );
	  }
	}
      

      // Complex backoff.  Changes the segment stream to the right,
      // may still leave a discontinuity to the left. This could be
      // fixed, but it would requires a better search. Rob's thoughts
      // are that the simple method works better, unless it resorts to
      // a bad default rule.

      //    while(!this->unitAvailable(diphone_name) && 
      //          diphone_backoff_rules && 
      //          !diphone_backoff_rules->backoff(it))
      //      diphone_name = EST_String::cat(it->S("name"),"_",it->next()->S("name"));
      
      if( !this->unitAvailable( diphone_name ) )
	missing_diphones.append( diphone_name );
      
      EST_Item *t = units->append();
      t->set( "name", diphone_name );
      t->set_val( "ph1", est_val(it) );
    }

  // stop if necessary units are still missing.
  if( missing_diphones.length() > 0 ){
    for( EST_Litem *it=missing_diphones.head(); it!=0 ; it=next(it) )
      printf( "requested diphone missing: %s\n", missing_diphones(it).str() );
 
    EST_error("Unable to synthesise utterance due to missing diphones");
  }

  // Make the decoder do its thing
  // -1 means number of states at each time point not fixed
  EST_Viterbi_Decoder v( getCandidatesFunction, extendPath, -1 );  

  // turn on pruning if necessary
  if( (pruning_beam>0) || (ob_pruning_beam>0) )
    v.set_pruning_parameters( pruning_beam, ob_pruning_beam );
 
  // temporary hack necessary because decoder can only take a 
  // function pointer (would be better to relax this restriction in 
  // the EST_Viterbi_Decoder class, or in a replacement class, rather
  // than using this hack)
  globalTempVoicePtr = this;

  v.set_big_is_good(false);

  if( verbosity() > 0 )
    v.turn_on_trace();

  v.initialise( units );
  v.search();

  // take hold of the best path (end thereof)
  EST_VTPath *bestp=0;
  if( !v.result( &bestp ) )
    EST_error( "No best candidate sequence found" );

  // fill in the best path features in the Unit Relation
  it = units->tail();

  for ( ; bestp != 0 && it != 0; bestp=bestp->from, it=it->prev() ){
    EST_Track *coefs = new EST_Track;
    CHECK_PTR(coefs);
    EST_Wave *sig = new EST_Wave;
    CHECK_PTR(sig);
    int midf;

    getDiphone( bestp->c, coefs, sig, &midf );

    EST_Item *firstPhoneInDiphone = bestp->c->s;
    it->set_val( "sig", est_val( sig ) );
    it->set_val( "coefs", est_val( coefs ) );
    it->set( "middle_frame", midf );
    it->set( "source_utt", firstPhoneInDiphone->relation()->utt()->f.S("fileid"));
    it->set_val( "source_ph1", est_val( firstPhoneInDiphone ));
    it->set( "source_end", firstPhoneInDiphone->F("end"));
    it->set( "target_cost", bestp->c->score );

    //have to recalculate join cost as it's not currently saved anywhere
    if( bestp->from == 0 )
      it->set( "join_cost", 0.0);
    else
      // join cost between right edge of left diphone and vice versa
      it->set( "join_cost", (*jc)( bestp->from->c->s->next(), bestp->c->s ) );
  }

  parse_diphone_times( *units, *segs );
}

bool DiphoneUnitVoice::unitAvailable( const EST_String &diphone ) const
{ 
  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin(voiceModules); it; it++ )
    if( (*it)->numAvailableCandidates(diphone) > 0 )
      return true;

  return false;
}

unsigned int DiphoneUnitVoice::numAvailableCandidates( const EST_String &diphone ) const
{
  unsigned int number = 0;
  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin(voiceModules); it; it++ )
    number += (*it)->numAvailableCandidates(diphone);
  
  return number;
}


////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
static void my_parse_diphone_times(EST_Relation &diphone_stream, 
				   EST_Relation &source_lab)
{
  EST_Item *s, *u;
  float dur1, dur2, p_time=0.0;
  
  for( s=source_lab.head(), u=diphone_stream.head(); u; u=next(u), s = next(s)){
    EST_Track *pm = track(u->f("coefs"));
    
    int end_frame = pm->num_frames() - 1;
    int mid_frame = u->I("middle_frame");
    
    dur1 = pm->t(mid_frame);
    dur2 = pm->t(end_frame) - dur1;
    
    s->set("source_end", (dur1+p_time));
    s->set("end", (dur1 + p_time)); // this is for copy synthesis
    
    p_time = s->F("source_end")+dur2;
    u->set("end", p_time);
  }
  if(s){
    s->set("source_end", (p_time));
    s->set("end", (p_time)); // this is for copy synthesis
  }
}

// special case of the above for utterances structures that are
// actually in the voice database, which doesn't do any search
// This is useful for doing copy synthesis of utterances (eg.
// to test out resynthesis, prosodic modification and so on)
void DiphoneUnitVoice::getCopyUnitUtterance( const EST_String &utt_fname, 
					     EST_Utterance **utt_out ) const
{
  // need to find which, if any, voice module has this utterance
  // in its list
  EST_TList<DiphoneVoiceModule*>::Entries module_iter;
  EST_Utterance *db_utt=0;
  for( module_iter.begin(voiceModules); module_iter; module_iter++ )
    if( (*module_iter)->getUtterance(&db_utt, "fileid", utt_fname) == true )
      break;

  if( db_utt == 0 )
    EST_error( "Could not find Utterance %s in any voice module",
	       utt_fname.str() );
  else{
    // deep copy database utterance and fill in Unit relation
    *utt_out = new EST_Utterance( *db_utt );
    CHECK_PTR(utt_out);

    EST_Utterance myUtt( *db_utt );

    cerr << myUtt.relation_present( "Segment" ) << " "
	 << myUtt.num_relations() <<endl;
    

    cerr << db_utt->relation_present( "Segment" ) << " "
	 << (*utt_out)->relation_present( "Segment" ) << " "
	 << (*utt_out)->num_relations() <<endl;


    EST_Relation *segs = (*utt_out)->relation( "Segment" );
    EST_Relation *units = (*utt_out)->create_relation( "Unit" );
  
    // Initialise the Unit relation + fill in necessary/suitable
    // synthesis parameters
    EST_String ph1, ph2;
    EST_Item *it = segs->tail();
    EST_Item *db_utt_seg_it = db_utt->relation( "Segment" )->tail();
    if( it == 0 )
      EST_error( "Segment relation is empty" );
    else{
      ph2 = it->S("name");    
      while( ((it=it->prev())!=0) && 
	     ((db_utt_seg_it=db_utt_seg_it->prev())!=0) ){
	EST_Track *coefs = new EST_Track;
	CHECK_PTR(coefs);
	EST_Wave *sig = new EST_Wave;
	CHECK_PTR(sig);
	int midf;
	
	(*module_iter)->getDiphone( db_utt_seg_it, coefs, sig, &midf );
	
	ph1 = it->S("name");
	EST_Item *t = units->prepend();
	t->set( "name", EST_String::cat(ph1,"_",ph2) );
	t->set_val( "ph1", est_val(it) );
	t->set_val( "sig", est_val( sig ) );
	t->set_val( "coefs", est_val( coefs ) );
	t->set( "middle_frame", midf );
	t->set( "source_utt", db_utt->f.S("fileid"));
	t->set_val( "source_ph1", est_val( db_utt_seg_it ));
	t->set( "source_end", db_utt_seg_it->F("end"));
	t->set( "target_cost", 0.0 );
	t->set( "join_cost", 0.0);
	
	ph2 = ph1;
      }
    }
    my_parse_diphone_times( *units, *segs );
  }  
}
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////



unsigned int DiphoneUnitVoice::numUnitTypes() const
{
  //necessary?
  return 0;
}

unsigned int DiphoneUnitVoice::numDatabaseUnits() const
{
  unsigned int sum=0;
  
  EST_TList<DiphoneVoiceModule*>::Entries it;
  
  for( it.begin( voiceModules ); it; it++ )
    sum += (*it)->numModuleUnits();
  
  return sum;
}


//////////////////////////////////////////////////////////////////////////

void DiphoneUnitVoice::set_diphone_backoff(DiphoneBackoff *dbo)
{
  if (diphone_backoff_rules)
    delete diphone_backoff_rules;
  diphone_backoff_rules = dbo;
}


int DiphoneUnitVoice::getPhoneList( const EST_String &phone, ItemList &list )
{
  unsigned int n=0;

  EST_TList<DiphoneVoiceModule*>::Entries it;
  for( it.begin( voiceModules ); it; it++ )
    n += (*it)->getPhoneList( phone, list );

  return n;
}



void DiphoneUnitVoice::precomputeJoinCosts( const EST_StrList &phones, bool verbose  )
{
  EST_StrList::Entries it;
  for( it.begin( phones ); it; it++ ){
    ItemList *l = new ItemList;
    CHECK_PTR(l);

    unsigned int n = getPhoneList( (*it), *l );
    
    if( verbose==true )
      cerr << "phone " << (*it) << "  "  << n << " instances\n";
      
    if( n>0 ){
      jc->computeAndCache( *l, true ); //verbose=true
    }
    else
      EST_warning( "Phone %s not listed in voice", (*it).str() );

    delete l;
  }
}
