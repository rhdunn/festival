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


#ifndef __DIPHONEUNITVOICE_H__
#define __DIPHONEUNITVOICE_H__

#include "VoiceBase.h"
#include "DiphoneBackoff.h"
#include "siod_defs.h"
#include "EST_Val_defs.h"
#include "EST_String.h"

#include "EST_types.h" // for EST_StrList

class EST_Utterance;
class EST_Relation;
class EST_VTCandidate;
class EST_VTPath;
class EST_Features;
class EST_Track;
class EST_Wave;
class EST_Item;
class DiphoneVoiceModule;
class EST_JoinCost;
class EST_TargetCost;

#include "EST_THash.h"
template<class T> class EST_TList;
typedef EST_TList<EST_Item*> ItemList;


SIOD_REGISTER_CLASS_DCLS(du_voice,DiphoneUnitVoice)
VAL_REGISTER_CLASS_DCLS(du_voice,DiphoneUnitVoice)


class DiphoneUnitVoice : public VoiceBase {
public:
  DiphoneUnitVoice( const EST_StrList& basenames,
		    const EST_String& uttDir,
		    const EST_String& wavDir,
		    const EST_String& pmDir,
		    const EST_String& coefDir,
		    unsigned int srate = 16000,
		    const EST_String& uttExt  = ".utt",
		    const EST_String& wavExt  = ".wav",
		    const EST_String& pmExt   = ".pm", 
		    const EST_String& coefExt = ".coef" );
  
  virtual ~DiphoneUnitVoice();
  
  virtual void initialise();
  virtual unsigned int numDatabaseUnits() const;
  virtual unsigned int numUnitTypes() const;

  virtual bool synthesiseWave( EST_Utterance *utt );

  virtual void getUnitSequence( EST_Utterance *utt );

  void getCopyUnitUtterance( const EST_String &utt_fname, 
			     EST_Utterance **utt_out ) const;

  EST_VTCandidate* getCandidates( EST_Item *s, EST_Features &f ) const;

  virtual bool unitAvailable( const EST_String &diphone ) const;
  virtual unsigned int numAvailableCandidates( const EST_String &unit ) const;

  unsigned int numModules() const { return voiceModules.length(); }
  
  bool addVoiceModule( const EST_StrList& basenames,
		       const EST_String& uttDir,
		       const EST_String& wavDir,
		       const EST_String& pmDir,
		       const EST_String& coefDir,
		       unsigned int srate = 16000,
		       const EST_String& uttExt  = ".utt",
		       const EST_String& wavExt  = ".wav",
		       const EST_String& pmExt   = ".pm",
		       const EST_String& coefExt = ".coef" );


  // assume responsibility to delete vm when done with it
  void registerVoiceModule( DiphoneVoiceModule *vm );

  // del=true means it's ok to delete the join cost when we're done
  // with it
  void setJoinCost( EST_JoinCost *jcost, bool del=false );
  const EST_JoinCost& getJoinCostCalculator( ) const { return *jc; }

  void setTargetCost( EST_TargetCost *tcost, bool del=false );
  const EST_TargetCost& getTargetCostCalculator( ) const { return *tc; }

  void  set_pruning_beam( float width ) { pruning_beam=width; }
  float get_pruning_beam( ) const { return pruning_beam; } 
  void  set_ob_pruning_beam( float width ){ ob_pruning_beam=width; }
  float get_ob_pruning_beam( ) const { return ob_pruning_beam; }
  
  void set_wav_samplerate( unsigned int sr ) { wav_srate = sr; }
  unsigned int get_wav_samplerate( ) const { return wav_srate; }

  void precomputeJoinCosts( const EST_StrList &phones, bool verbose=true );


private:
  // don't allow copying of Voices (for now?)
  DiphoneUnitVoice( const DiphoneUnitVoice& );
  DiphoneUnitVoice& operator=( const DiphoneUnitVoice& );
  
  void addToCatalogue( const EST_Utterance *utt ); 

  void getDiphone( const EST_VTCandidate *cand, 
		   EST_Track* coef, EST_Wave* sig, int *midframe );

  int getPhoneList( const EST_String &phone, ItemList &list );

private:
  EST_TList<DiphoneVoiceModule*> voiceModules;
  float pruning_beam;     // beam pruning
  float ob_pruning_beam;  // observation beam pruning

  unsigned int wav_srate;

  EST_JoinCost *jc;
  bool jc_delete;

  EST_TargetCost *tc;
  bool tc_delete;

private:
  DiphoneBackoff *diphone_backoff_rules;   // diphone backoff rules
  
public:
  void set_diphone_backoff(DiphoneBackoff *dbo);

};

#endif // __DIPHONEUNITVOICE_H__

