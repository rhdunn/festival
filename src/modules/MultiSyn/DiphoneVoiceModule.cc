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
/* A diphone unit selection "voice module"                               */
/* (implemented using a list of utterance objects)                       */
/*************************************************************************/

#include "DiphoneVoiceModule.h"
#include "EST_TargetCost.h"
#include "EST_viterbi.h"
#include "EST_rw_status.h"
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

#include "EST_Val.h"

// from src/modules/UniSyn_diphone/us_diphone.h
// this won't be staying here long...
void parse_diphone_times(EST_Relation &diphone_stream, 
			 EST_Relation &source_lab);

SIOD_REGISTER_CLASS(du_voicemodule,DiphoneVoiceModule)
VAL_REGISTER_CLASS(du_voicemodule,DiphoneVoiceModule)

VAL_REGISTER_CLASS(diphonevoicemoduleptr,DiphoneVoiceModulePtr)


DiphoneVoiceModule::DiphoneVoiceModule( const EST_StrList& basenames,
					const EST_String& uttDir,
					const EST_String& wavDir,
					const EST_String& pmDir,
					const EST_String& coefDir,
					unsigned int sr,
					const EST_String& uttExt,
					const EST_String& wavExt,
					const EST_String& pmExt,
					const EST_String& coefExt )
					
  : fileList( basenames ),
    utt_dir ( uttDir  ),
    utt_ext ( uttExt  ),
    pm_dir( pmDir  ),
    pm_ext( pmExt  ),
    coef_dir( coefDir ),
    coef_ext( coefExt ),
    wave_dir( wavDir  ),
    wave_ext( wavExt  ),
    wav_srate( sr ),
    utt_dbase( 0 ),
    catalogue( 0 )
{
  
}

void DiphoneVoiceModule::addCoefficients( EST_Relation *segs, const EST_Track& coefs )
{
  EST_FVector *f;

  for( EST_Item *seg=segs->head(); seg!=0; seg=seg->next() ){

    float midt;
    if (seg->f_present("cl_end")) // join at cl_end point for stops
      midt = seg->F("cl_end");
    else if (seg->f_present("dipth")) // join at 25% through a diphthong
      midt =  0.75*seg->F("start") + 0.25*seg->F("end");
    else
      midt = (seg->F("start")+seg->F("end"))/2;
    
    f = new EST_FVector;
    CHECK_PTR(f);
    coefs.copy_frame_out(coefs.index(midt), *f);    
    
    seg->set_val( "midcoef", est_val(f) );
  }
}

void DiphoneVoiceModule::initialise()
{
  EST_Utterance *u=0;
  EST_Relation *segs=0;

  utt_dbase = new EST_TList<EST_Utterance *>;
  CHECK_PTR(utt_dbase);
  
  catalogue = new EST_TStringHash<ItemList*>( 2500 );
  CHECK_PTR(catalogue);

  int numIgnoredPhones=0;

  for( EST_Litem *it=fileList.head(); it!=0 ; it=next(it) ){
    u = new EST_Utterance;
    CHECK_PTR(u);
    
    if( (u->load(utt_dir+fileList(it)+utt_ext)) != read_ok )
      EST_error( "Couldn't load utterance %s\n", 
		 (const char*)fileList(it) );
    
    segs = u->relation( "Segment" );
    
    // temporary hack in case utterance files don't
    // have pauses marked at start and end
    //       EST_Item *endItem = segs->last();
    //       if( endItem->S( "name" ) != "pau" ){
    // 	EST_Item *item = segs->append();
    // 	item->set( "name", "pau" );
    // 	item->set_function( "start", "standard+unisyn_start" );
    // 	item->set( "end", endItem->F("end")+0.1 );
    //       }
    
    //       EST_Item *firstItem = segs->first();
    //       if( firstItem->S( "name" ) != "pau" ){
    // 	EST_Item *item = segs->prepend();
    // 	item->set( "name", "pau" );
    // 	item->set( "end", firstItem->F("start") );
    // 	item->set( "start", firstItem->F("start")-0.1 );
    //       }
    
    // add join cost coefficients (at middle of phones)
    EST_Track coefs;
    if( (coefs.load((coef_dir+fileList(it)+coef_ext))) != read_ok )
      EST_error( "Couldn't load data file %s", 
		 (const char*) (coef_dir+fileList(it)+coef_ext) );      
    
    addCoefficients( segs, coefs );
    
    addToCatalogue( u, &numIgnoredPhones );
    utt_dbase->append( u );
  }
  
  EST_warning( "Ignored %d phones with bad flag set\n", numIgnoredPhones ); 
}

DiphoneVoiceModule::~DiphoneVoiceModule()
{
  if( utt_dbase != 0 ){
    EST_Litem *it = utt_dbase->head();
    for( ; it!=0 ; it=next(it) )
      delete (*utt_dbase)(it);
    delete utt_dbase;
  }
  
  delete catalogue;
}

void DiphoneVoiceModule::addToCatalogue( const EST_Utterance *utt, int *num_ignored )
{
  EST_Item *item;
  ItemList *diphoneList;
  EST_String ph1, ph2;
  int found=0;
  
  item = (utt->relation( "Segment" ))->tail();
  if( item!=0 ){
    ph2 = item->S("name");
    
    while( (item=item->prev()) != 0 ){

      if( item->f_present( "bad" ) ){

	(*num_ignored)++;

// 	EST_warning( "Ignoring phone \"%s\" (%s, %fs, bad flag \"%s\")", 
// 		     item->S("name").str(), 
// 		     utt->f.S("fileid").str(), 
// 		     item->F("end"),
// 		     item->S("bad").str() );
	
	if( (item=item->prev()) != 0 ){ //skip 2 diphones with this bad phone
	  ph2 = item->S("name");
	  continue;
	}
	else 
	  break; //already at start of list, so finish up
      }
      
      ph1 = item->S("name");
      
      diphoneList = catalogue->val(EST_String::cat(ph1,"_",ph2), found);
      
      if( !found ){
	diphoneList = new ItemList;
	CHECK_PTR(diphoneList);
	catalogue->add_item(EST_String::cat(ph1,"_",ph2), diphoneList, 1); // no_search=1
      }
     
      diphoneList->append( item );
      
      ph2 = ph1;
    }
  }
}

void DiphoneVoiceModule::getDiphone( const EST_Item *phone1, 
				     EST_Track* coef, EST_Wave* sig, int *midframe ) const
{
  EST_Item *phone2 = phone1->next();
	
  // load the relevant parts
  EST_String fname = phone1->relation()->utt()->f.S("fileid");

  float startt,midt,endt;

  startt = phone1->f_present("cl_end") ? phone1->F("cl_end")
    : ( phone1->F("end")+phone1->F("start") )/2;
  midt = phone1->F("end");
  endt = phone2->f_present("cl_end") ? phone2->F("cl_end")
    : ( phone2->F("end")+phone2->F("start") )/2;

  // get pitchmarks for pitch synchronous synthesis
  EST_Track *tempcoef = new EST_Track;
  CHECK_PTR(tempcoef);
  if( (tempcoef->load((pm_dir+fname+pm_ext))) != read_ok )
    EST_error( "Couldn't load data file %s", 
	       (const char*) (pm_dir+fname+pm_ext) );

  // following few lines effectively moves segment boundaries to
  // line up with pitch periods. 
  int copy_start = tempcoef->index( startt );
  int copy_end   = tempcoef->index( endt );
  copy_end -= 1; //so that adjacent units don't start and end with same frame
  int copy_len   = copy_end - copy_start + 1;
  startt = tempcoef->t( copy_start );
  endt = tempcoef->t( copy_end );

  if( copy_len == 0 ){
    EST_warning( "%s(%f->%f): %s_%s diphone length means 1 pitchmark will be duplicated",
		 fname.str(), startt, endt, phone1->S("name").str(), phone2->S("name").str() );
    copy_len=1;
  }
  else if( copy_len < 0 ){
    EST_error( "%s(%f->%f): %s_%s diphone length renders %d pitchmark",
	       fname.str(), startt, endt, phone1->S("name").str(), phone2->S("name").str(), copy_len );
  }

  tempcoef->copy_sub_track( *coef, copy_start, copy_len );

  *midframe = coef->index( midt );

  // adjust timing, which Festival synthesis code makes assumptions about
  // SPECIFICALLY, the unisyn module wants all units to start from
  // the first value above 0.0 (as the first pitch mark)
  float t_off = (copy_start!=0) ? tempcoef->t(copy_start-1) : 0.0;
  int nframes = coef->num_frames();
  for( int i=0; i<nframes; ++i )
    coef->t(i) -= t_off;

  //start waveform at previous pitchmark (this is period approximation used) 
  int st_sample = (int)rint( t_off * (float) wav_srate ); 

  //preferably end waveform at following pitchmark (follows convention in UniSyn module)
  int end_sample;
  if( copy_end+1 < tempcoef->num_frames() )
    end_sample = (int) rint( tempcoef->t(copy_end+1) * (float) wav_srate );
  else{
    // estimate from previous pitch mark shift
    int pp_centre_sample = (int) rint( endt * (float) wav_srate );
    int pp_first_sample  = (int) rint( tempcoef->t(copy_end-1) * (float) wav_srate );
    end_sample           = (2*pp_centre_sample)-pp_first_sample;    
  }

  // (obviously, we would want to load and cache any files         //
  // which haven't been loaded yet, rather than just load          //
  // the parts each and every time)                                //
  if( sig->load( wave_dir+fname+wave_ext,                          //
		 st_sample, end_sample-st_sample+1) != read_ok )   //
    EST_error( "Couldn't load data file %s",                       //
	       (const char*) (wave_dir+fname+wave_ext) );          //

  delete tempcoef;
}

int DiphoneVoiceModule::getCandidateList( const EST_Item& target, 
					  const EST_TargetCost& tc,
					  EST_VTCandidate **head,
					  EST_VTCandidate **tail ) const
{ 
  int nfound=0;
  EST_VTCandidate *c = 0;
  EST_VTCandidate *nextc = 0;
  ItemList::Entries it;
  
  int i=0,found=0;
  const ItemList *candidateItemList = catalogue->val( target.S("name"), found );
  if( found!=0 ){
    // iterate along ItemList and build a corresponding candidate list
    // (note the order will be reversed, because we use c->next)
    it.begin( *candidateItemList );

    *tail = c = new EST_VTCandidate; //keep hold of first one created
    CHECK_PTR(tail);

    // set up first candidate created (tail one)
    c->s = (*it);
    //    c->name = i; //c->name is an EST_Val;

    // probably an abuse (will become unnecessary with a more general candidate class)
    DiphoneVoiceModulePtr *p1 = new DiphoneVoiceModulePtr( this ); //deleted by EST_Val c->name 
    CHECK_PTR(p1);
    c->name = est_val( p1 ); // used later to get synthesis parameters

    c->score = tc( item(target.f("ph1")), (*it) );
    c->next = nextc;
    nextc = c;

    //continue iterating along list
    it++; i++; //first take into account that one has already been added
    for( ; it; it++, i++ ){
      c = new EST_VTCandidate;
      CHECK_PTR(c);
      c->s = (*it);
      //      c->name = i; //c->name is an EST_Val;
      
      // probably an abuse (will become unnecessary with a more general candidate class)
      DiphoneVoiceModulePtr *p2 = new DiphoneVoiceModulePtr( this ); //deleted by EST_Val c->name 
      c->name = est_val( p2 ); // used later to get synthesis parameters
      
      c->score = tc( item(target.f("ph1")), (*it) );
      c->next = nextc;
      nextc = c;
    }
    
    *head = c; // keep hold of last one set up

    nfound = i;
  }
  
  return nfound;
}

int DiphoneVoiceModule::getPhoneList( const EST_String &phone, ItemList &list )
{
  unsigned int n=0;

  if( utt_dbase != 0 ){
    for( EST_Litem *it=utt_dbase->head(); it!=0 ; it=next(it) ){
      EST_Item *ph=(*utt_dbase)(it)->relation("Segment")->head();
      for( ; ph!=0; ph=next(ph) ){
	if( ph->S("name") == phone ){
	  list.append( ph );
	  n++;
	}
      }
    }
  }

  return n;
}
 


bool DiphoneVoiceModule::getUtterance( EST_Utterance** utt, int n ) const
{
  if( n<0 || n>(utt_dbase->length()-1) )
    EST_error( "Utterance index out of bounds" );

  if( utt == 0 )
    EST_error( "Invalid utterance" );

  // deep copy the utterance in question
  *utt = new EST_Utterance( *(utt_dbase->nth(n)) );
  CHECK_PTR(utt);

  return true;
}


bool DiphoneVoiceModule::getUtterance( EST_Utterance **utt, 
				       const EST_String &feat_name,
				       const EST_Val &value ) const
{
  //search down list of utterance structures, comparing
  // fileid feature.  If find a match, return pointer to that
  // utterance.
  for( EST_Litem *it=utt_dbase->head(); it!=0 ; it=next(it) )
    if( (*utt_dbase)(it)->f.val(feat_name) == value ){
      *utt = (*utt_dbase)(it);
      return true;
    }

  return false;
} 


unsigned int DiphoneVoiceModule::numUnitTypes() const
{
  return catalogue ? catalogue->num_entries() : 0;
}

unsigned int DiphoneVoiceModule::numModuleUnits() const
{
  unsigned int sum=0;
  
  if( catalogue != 0 ){
    EST_TStringHash<ItemList*>::Entries it;
    
    for( it.begin( *catalogue ); it; it++ )
      sum += it->v->length(); //EST_UList.length() counts the entries :(
  }
  
  return sum;
}


unsigned int DiphoneVoiceModule::numAvailableCandidates( const EST_String &unit ) const
{
  int number=0;

  int found=0;
  const ItemList *candidateItemList = catalogue->val( unit, found );

  if( found > 0 )
    number = candidateItemList->length();
  
  return number;
}    
