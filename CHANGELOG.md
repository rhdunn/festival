# Change Log

## [1.4.1-1] - 2015-04-27 Maintenance

Add and modernize the standard project files:

  * Added a CHANGELOG.md file to track the project changes.
  * Incorporate the NEWS history into CHANGELOG.md.
  * Added a .gitignore file to ignore the build output.
  * Support building HTML versions of the markdown files with `kramdown`.

Support the GNU standard project layout:

  * Add an AUTHORS file.
  * Link NEWS to another file in the project.

Add autotools support:

  * Added an autogen.sh script to setup the configure script.
  * Backport configure.in from 1.4.2 as configure.ac.
  * Link to the automake files needed to run the configure script.
  * Use `autoconf` to generate the configure script.

Build system fixes:

  * `config/config.in`: Use the `ESTDIR` environment variable instead of
    hard-coding the Edinburgh Speech Tools location.

## [1.4.1] - 1999-11-21 Release

  * SSFF (for emulabel) track support
  * AIX support
  * Java fixes
  * various minor bug fixes
  * WFST with proper quoting
  * Wagon sample counts
  * gcc-2.95.1 support

## [1.4.0] - 1999-06-20 Release

  * becomes free software

## [1.3.95] - 1999-06-06 Beta

  * size/speed/memory leak overhaul (no memory leaks)
  * XML support for relation loading (for SOLE support)
  * JSAPI initial support
  * GalaxyCommunicator architecture interface
  * `ked_mttilt_diphone` voice built
  * Parser trained on MARSEC (prosodic) brackets rather than syntax
  * `Unisyn_selection` fully integrated
  * `Unisyn_phonology` fully integrated 
  * viterbi cart/ngram/wfst base LTS prediction (did improve but BIG)
  * viterbi cart/ngram based accent prediction (didn't improve)
  * tilt working (again)
  * audioin (`na_record`) for many architectures
  * viterbi from Scheme (with cart, ngram, wfst models)

## [1.3.1] - 1999-01-26 Release

  * egcs-1.1.1 support
  * `tobi_rules` update (GM)
  * replace readline with editline (+ extensions)
  * Lots of little bug fixes
  * cluster code tidied up
  * kal voice
  * ked power normalization
  * updated lexicons with addenda for US and UK 
  * New LTS models for US and UK English
  * "Building Voices in Festival" document

## [1.3.0] - 1998-08-24 Release

  * UniSun/groupfile optimizations
  * Java client support
  * Fixed ESPS so both track and wave output works
  * Retraining of most modules with new architecture (durations improved)
  * rxp, (Richard's XML parser) integrated and Sable XMLified
  * Fringe display program for labels and utterances
  * Metrical tree synthesis
  * A new utterance architecture (Relations and Items)
  * utterance save and load work properly now
  * Trainable LTS system   
  * Lexicon cache system
  * Substantial optimization of front end (twice the speed)
  * UniSyn, new signal processing and generic waveform synthesis module
  * OLS code added
  * WFST support for kk rules, regular grammars etc, simple English morphology

## [1.2.4] - 1997-11-30 Beta

  * Tilt analysis and Tilt intonation modules added.
  * `make_utts` substantially improved (> 100 times faster)
  * text2wave script added
  * Pitch synchronous lpc analysis and support
  * rab consonant clusters labelled
  * New duration tree (wagon stepwise) much smaller if not better
  * SCFG grammar and parser (`scfg_parse_text` added as festival script)
  * change config stuff (again)

## [1.2.1] - 1997-10-01 Release

  * preliminary support for Visual C++
  * Use path-append rather than string-append (in buckets of places)
  * Minor bugs fixes throughout the code (end silences are now *always*
    inserted in tts)
  * Linux socket bug fixed (`get_url` didn't work)
  * native irix audio support

## [1.2.0] - 1997-09-05 Release

  * Proclaim modules and voices
  * automatic detection of voices
  * Phonset, lexicon, ltsrules listing and printing
  * 16 bit linear native support for Solaris i386 (sb16)
  * Update Festival Tutorial to 1.2.0

## [1.1.99] - 1997-08-15 Beta

  * Win NT (and 95) initial support Cygnus win32 and Visual C++
  * 100 more pages of documentation
  * LPC analysis for voices now ESPS independent
  * Spanish el voices tidy up (Borja)
  * ToBI by rule implementation
  * Confirmed support for gcc-2.7.2, gcc-2.6.3, Linux, FreeBSD, SunOS
    Alpha and SunCC port on Solaris
  * reference card added
  * return s-expressions in server/client mode
  * OGI markup mode added.
  * Native support for sun16, linux16 and freebsd (compile time option)
  * Changed names of .C files to .cc files for bILL  
  * wagon integrated into speech tools (plus docs)
  * auto-text-mode-alist for automatic selection of text mode from file name
  * Associated token tests added
  * Many more tokens dealt with (numbers, money, roman, phone, etc.)
    (analysed databases to see what coverage is like)
  * A probablistic chart parser (no significant grammars though)
  * RJC's new database/units/join/modify modules taking shape
  * Some more examples added to the tutorial (with answers)
  * Integrated CSLU changes for OGItoolkit including TCL support
  * stml support for phrase types and words inline
  * ssml -> stml
  * Postlexical rules done in Scheme rather than C++
  * Rest of functions to allow any manipulation of utterance from scheme
  * New duration models trained for both English and American
  * New lexicon (CMU based)
  * Consonant cluster support (for kd)
  * American diphone set   
  * Cluster unit selection algorithm more robust
  * Ngram backoff smoothing
  * Token pos, for numbers (97.5%) but does poor on phone numbers
  * New lexicon with final Rs and r deletion as postlex rule
  * Update pos prediction (ts39) and phrase break ngrams (faster to load)
  * New ngram format (binary files, and smoothing)
  * Vowel reduction module
  * Sun CC port
  * New string class (rjc) remove dependence on libg++
  * Update of course notes and new section on building models from dbs
  * Yarowsky homograph disambiguation

## [1.1.1] - 1997-01-24 Release

This is the first public release:

  * a number of configuration and INSTALL documentation bugs fixed
  * SSML tidied up and a festival script provided for it.
  * Diphones, again, checked and copyright explicitly added

## [1.1.0] - 1997-01-06 Release

  * Roger diphones now default speaker
  * A new unit clustering algorithm with acoustic costs and
    optimal coupling
  * BSD socket client/server support
  * A format function in Scheme (fprintf-like)
  * A short course on Speech Synthesis in Festival 
    (with course notes and exercises)
  * A programmable form of text modes including externally customizable
    token to word rules.
  * Fully programmable intonation module (for ToBI-like theories)
  * Backtrace facility in Lisp
  * Externally specified Utterance end (for all tts modes) with lookahead
  * Roger diphones, first draft

## [1.0.0] - 1996-11-08 Release

  * Substantial bug fixes, stabilization and documentation updates
  * Added residual excited LPC synthesizer and removed PSOLA code.
  * Made sucs and taylor optional modules, new modules
    can be added without modifying the base code
  * MOBY lexicon (not as good as cuvoald but free)
  * New diphone grouping software
  * A new diphone database module (free from adc)

## [0.1] - 1996-09-30 Release

  * MBROLA support (good example of external module)
  * latest news: read out the latest news (from Time Warner, Pathfinder)
  * audio spooler
  * `--language` option on command line
  * Spanish synthesis
  * Letter to sound rules as external system (replacing all the NRL code)
  * Welsh synthesis, making the whole system more language independent
  * sucs spoke in reasonably way (`gsw_450` and f2b dbs)
  * document strings for functions (built in and user) and variables
    access from command line and dumped automatically into texinfo
  * cleaned up SSML implementation 
  * break prediction integrated using viterbi and pos
  * sucs module started (selection of units for concatenative synthesis)
  * a part-of-speech tagging system (ngram/viterbi based)
  * viterbi code added
  * fixes in SIOD for running batch and stdin, also `sub_prompts` added
  * saytime example
  * Memory leaks fixed, no leaks for tts  

## [0.0] - 1996-07-30 Release

  * a significiant start at documentation (texinfo -> info & html)
  * festival scripts using `#!` on first line
  * donovan diphone support 
  * can compile (with too many warnings) under g++ 2.7.2
  * copyrights on all files
  * memory leak checks (only 8 bytes for "unknown" words)
  * SSML (and tts file modes)
  * cuvoald cmu and beep lexicons
  * lexicon compilation
  * web page, emacs interface

## 1996-06-02

  * Klatt duration module
  * syllabification in phones from letter to sound rules
  * Linear Regression model for F0 prediction (from ToBI labels)
  * CART (wagon) built trees for duration (zscores), phrase boundaries, 
    accent and endtone prediction.
  * ffeatures allowing specification of features of an utterance

## 1996-05

  * integrated Taylor diphone module
  * US Naval Research letter to sound rules
  * CSTR lexicon

## 1996-04-12

first words "hello"

Start with `speech_tools` library, scheme-in-one-defun and readline
and external CSTR diphone synthesizer.

## 1996-04-07

Work started.
