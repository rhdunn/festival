;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                         Copyright (c) 1997                            ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;;  this software and its documentation without restriction, including   ;;
;;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;;  the following conditions:                                            ;;
;;;   1. The code must retain the above copyright notice, this list of    ;;
;;;      conditions and the following disclaimer.                         ;;
;;;   2. Any modifications must be clearly marked as such.                ;;
;;;   3. Original authors' names are not deleted.                         ;;
;;;   4. The authors' names are not used to endorse or promote products   ;;
;;;      derived from this software without specific prior written        ;;
;;;      permission.                                                      ;;
;;;                                                                       ;;
;;;  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ;;
;;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
;;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ;;
;;;  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ;;
;;;  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ;;
;;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;;  THIS SOFTWARE.                                                       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Postlexical rules
;;;

(define (PostLex utt)
"(PostLex utt)
Apply post lexical rules to segment stream.  These may be almost
arbitrary rules as specified by the particular voice, through the
postlex_hooks variable.  A number of standard post lexical rule
sets are provided including reduction, posessives etc.  These
rules are also used to mark standard segments with their cluster
information used in creating diphone names."
(let ((rval (apply_method 'PostLex_Method utt)))
  (cond
   (rval rval) ;; new style 
   (t   ;; should only really need this one
    (apply_hooks postlex_rules_hooks utt)))
  utt
))

(define (Classic_PostLex utt)
  "(Classic_PostLex utt)
Apply post lexical rules (both builtin and those specified in 
postlex_rules_hooks)."
  (Builtin_PostLex utt)  ;; haven't translated all the rules yet
  (apply_hooks postlex_rules_hooks utt)
  utt
)  

(defvar postlex_rules_hooks nil
"postlex_rules_hooks
A function or list of functions which encode post lexical rules.
This will be voice specific, though some rules will be shared across
languages.")

;;;  Mapping of full vowels to reduced vowels, this should be part
;;;  of the phoneset definitions
(defvar postlex_vowel_reduce_table
  '((mrpa
     ((uh @) (i @) (a @) (e @) (u @) (o @) (oo @)))
    (radio
     ((ah ax el en em) 
      (ih ax) 
;      (er axr ax) 
;      (iy ih) 
;      (ey ax) 
      (aa ax) 
      (ae ax) 
      (eh ax))))
"postlex_vowel_reduce_table
Mapping of vowels to their reduced form.  This in an assoc list of
phoneset name to an assoc list of full vowel to reduced form.")

(defvar postlex_vowel_reduce_cart_tree nil
"postlex_vowel_reduce_cart_tree
CART tree for vowel reduction.")

(defvar postlex_vowel_reduce_cart_tree_hand
  '((stress is 0)
    ((p.syl_break < 2)
     ((syl_break < 2)
      ((1))
      ((0)))
     ((0)))
    ((0)))
"postlex_vowel_reduce_cart_tree_hand
A CART tree for vowel reduction.  This is hand-written.")

(defvar postlex_vowel_reduce_cart_data
'
((R:SylStructure.parent.gpos is cc)
 (((0 0.993548) (1 0.00645161) 0))
 ((p.R:SylStructure.parent.gpos is md)
  (((0 0.903226) (1 0.0967742) 0))
  ((p.R:SylStructure.parent.gpos is det)
   ((n.R:SylStructure.parent.gpos is content)
    ((last_accent < 2.5)
     ((next_accent < 2.5)
      ((next_accent < 1.2)
       ((n.syl_break is 4)
        (((0 0.967213) (1 0.0327869) 0))
        ((syl_break is 4)
         (((0 0.952381) (1 0.047619) 0))
         ((n.syl_break is 4)
          (((0 0.953488) (1 0.0465116) 0))
          ((position_type is single)
           (((0 0.947368) (1 0.0526316) 0))
           ((accented is 0)
            ((n.accented is 0)
             (((0 0.857143) (1 0.142857) 0))
             (((0 0.415385) (1 0.584615) 1)))
            (((0 0.974359) (1 0.025641) 0)))))))
       (((0 0.968254) (1 0.031746) 0)))
      (((0 0.969697) (1 0.030303) 0)))
     (((0 0.976744) (1 0.0232558) 0)))
    (((0 0.990291) (1 0.00970874) 0)))
   ((next_accent < 108.5)
    ((p.R:SylStructure.parent.gpos is pps)
     (((0 0.828947) (1 0.171053) 0))
     ((R:SylStructure.parent.gpos is det)
      ((accented is 0)
       (((0 0.0599572) (1 0.940043) 1))
       (((0 0.949367) (1 0.0506329) 0)))
      ((p.R:SylStructure.parent.gpos is cc)
       (((0 0.880952) (1 0.119048) 0))
       ((p.R:SylStructure.parent.gpos is wp)
        (((0 0.875) (1 0.125) 0))
        ((p.R:SylStructure.parent.gpos is in)
         ((n.syl_break is 4)
          (((0 0.961538) (1 0.0384615) 0))
          ((next_accent < 2.5)
           ((syl_break is 4)
            (((0 0.95122) (1 0.0487805) 0))
            ((next_accent < 1.2)
             ((accented is 0)
              ((n.stress is 0)
               (((0 0.788462) (1 0.211538) 0))
               ((R:SylStructure.parent.R:Word.p.gpos is content)
                (((0 0.863636) (1 0.136364) 0))
                ((position_type is single)
                 (((0 0.729167) (1 0.270833) 0))
                 (((0 0.4) (1 0.6) 1)))))
              (((0 0.983871) (1 0.016129) 0)))
             (((0 0.96) (1 0.04) 0))))
           (((0 0.963636) (1 0.0363636) 0))))
         ((position_type is single)
          ((syl_break is 4)
           (((0 0.993865) (1 0.00613497) 0))
           ((p.R:SylStructure.parent.gpos is to)
            (((0 0.984375) (1 0.015625) 0))
            ((syl_break is 1)
             ((accented is 0)
              ((n.R:SylStructure.parent.gpos is in)
               (((0 0.869565) (1 0.130435) 0))
               ((R:SylStructure.parent.gpos is content)
                (((0 0.861789) (1 0.138211) 0))
                ((p.R:SylStructure.parent.gpos is content)
                 ((p.syl_break is 4)
                  (((0 0.858065) (1 0.141935) 0))
                  ((R:SylStructure.parent.gpos is in)
                   ((p.syl_break is 1)
                    ((n.R:SylStructure.parent.gpos is det)
                     (((0 0.659574) (1 0.340426) 0))
                     ((p.stress is 0)
                      (((0 0.422222) (1 0.577778) 1))
                      (((0 0.582278) (1 0.417722) 0))))
                    ((n.accented is 0)
                     ((n.R:SylStructure.parent.gpos is content)
                      (((0 0.65) (1 0.35) 0))
                      ((p.stress is 0)
                       (((0 0.464286) (1 0.535714) 1))
                       (((0 0.538462) (1 0.461538) 0))))
                     (((0 0.803279) (1 0.196721) 0))))
                   ((n.R:SylStructure.parent.gpos is det)
                    (((0 0.952381) (1 0.047619) 0))
                    ((n.syl_break is 4)
                     (((0 0.833333) (1 0.166667) 0))
                     ((p.stress is 0)
                      ((p.syl_break is 1)
                       ((n.syl_break is 1)
                        (((0 0.740741) (1 0.259259) 0))
                        ((R:SylStructure.parent.gpos is aux)
                         (((0 0.478261) (1 0.521739) 1))
                         (((0 0.769231) (1 0.230769) 0))))
                       (((0 0.755556) (1 0.244444) 0)))
                      (((0 0.797619) (1 0.202381) 0)))))))
                 (((0 0.870968) (1 0.129032) 0)))))
              (((0 0.983806) (1 0.0161943) 0)))
             (((0 0.977778) (1 0.0222222) 0)))))
          ((next_accent < 21.6)
           ((p.stress is 0)
            ((R:SylStructure.parent.R:Word.p.gpos is md)
             (((0 0.961538) (1 0.0384615) 0))
             ((position_type is mid)
              (((0 0.977612) (1 0.0223881) 0))
              ((n.R:SylStructure.parent.gpos is det)
               (((0 0.916667) (1 0.0833333) 0))
               ((R:SylStructure.parent.R:Word.n.gpos is 0)
                (((0 0.915493) (1 0.084507) 0))
                ((R:SylStructure.parent.R:Word.n.gpos is pps)
                 (((0 0.884615) (1 0.115385) 0))
                 ((n.stress is 0)
                  ((n.syl_break is 4)
                   (((0 0.986755) (1 0.013245) 0))
                   ((p.syl_break is 4)
                    (((0 0.977011) (1 0.0229885) 0))
                    ((n.syl_break is 4)
                     (((0 0.965517) (1 0.0344828) 0))
                     ((last_accent < 1.2)
                      ((last_accent < 0.1)
                       (((0 0.910448) (1 0.0895522) 0))
                       ((next_accent < 1.2)
                        ((R:SylStructure.parent.R:Word.n.gpos is in)
                         (((0 0.82) (1 0.18) 0))
                         ((n.syl_break is 0)
                          ((R:SylStructure.parent.R:Word.p.gpos is content)
                           (((0 0.819672) (1 0.180328) 0))
                           (((0 0.444444) (1 0.555556) 1)))
                          (((0 0.785714) (1 0.214286) 0))))
                        (((0 0.836364) (1 0.163636) 0))))
                      (((0 0.962025) (1 0.0379747) 0))))))
                  ((stress is 0)
                   ((n.syl_break is 4)
                    (((0 0.21875) (1 0.78125) 1))
                    ((R:SylStructure.parent.R:Word.p.gpos is aux)
                     (((0 0.259259) (1 0.740741) 1))
                     ((p.syl_break is 1)
                      (((0 0.243094) (1 0.756906) 1))
                      ((R:SylStructure.parent.R:Word.p.gpos is det)
                       (((0 0.290323) (1 0.709677) 1))
                       ((R:SylStructure.parent.R:Word.p.gpos is in)
                        (((0 0.3) (1 0.7) 1))
                        ((syl_break is 1)
                         (((0 0.289157) (1 0.710843) 1))
                         ((p.syl_break is 4)
                          (((0 0.352941) (1 0.647059) 1))
                          ((n.syl_break is 0)
                           (((0 0.311475) (1 0.688525) 1))
                           ((syl_break is 4)
                            (((0 0.4) (1 0.6) 1))
                            (((0 0.581395) (1 0.418605) 0)))))))))))
                   (((0 1) (1 0) 0)))))))))
            ((stress is 0)
             ((R:SylStructure.parent.R:Word.n.gpos is 0)
              (((0 0.121212) (1 0.878788) 1))
              ((next_accent < 2.4)
               ((R:SylStructure.parent.gpos is content)
                ((position_type is mid)
                 (((0 0.176895) (1 0.823105) 1))
                 ((p.syl_break is 1)
                  (((0 0.229167) (1 0.770833) 1))
                  ((syl_break is 4)
                   (((0 0.242775) (1 0.757225) 1))
                   ((p.syl_break is 0)
                    ((n.R:SylStructure.parent.gpos is in)
                     (((0 0.253521) (1 0.746479) 1))
                     ((R:SylStructure.parent.R:Word.p.gpos is in)
                      (((0 0.262774) (1 0.737226) 1))
                      ((last_accent < 2.1)
                       ((n.R:SylStructure.parent.gpos is aux)
                        (((0 0.304348) (1 0.695652) 1))
                        ((next_accent < 1.2)
                         ((n.R:SylStructure.parent.gpos is cc)
                          (((0 0.291667) (1 0.708333) 1))
                          ((syl_break is 1)
                           ((n.syl_break is 4)
                            (((0 0.344828) (1 0.655172) 1))
                            ((R:SylStructure.parent.R:Word.p.gpos is det)
                             (((0 0.364706) (1 0.635294) 1))
                             ((n.syl_break is 4)
                              (((0 0.384615) (1 0.615385) 1))
                              ((last_accent < 1.2)
                               ((p.accented is 0)
                                (((0 0.584906) (1 0.415094) 0))
                                ((n.accented is 0)
                                 ((R:SylStructure.parent.R:Word.p.gpos is content)
                                  (((0 0.41) (1 0.59) 1))
                                  (((0 0.6) (1 0.4) 0)))
                                 (((0 0.333333) (1 0.666667) 1))))
                               (((0 0.380952) (1 0.619048) 1))))))
                           ((p.accented is 0)
                            (((0 0.183673) (1 0.816327) 1))
                            ((n.R:SylStructure.parent.gpos is content)
                             ((n.stress is 0)
                              (((0 0.295455) (1 0.704545) 1))
                              ((R:SylStructure.parent.R:Word.p.gpos is content)
                               ((n.syl_break is 1)
                                (((0 0.5) (1 0.5) 0))
                                (((0 0.40625) (1 0.59375) 1)))
                               (((0 0.333333) (1 0.666667) 1))))
                             (((0 0.2) (1 0.8) 1))))))
                         (((0 0.3) (1 0.7) 1))))
                       (((0 0.302326) (1 0.697674) 1)))))
                    (((0 0.25) (1 0.75) 1))))))
                (((0 0.173913) (1 0.826087) 1)))
               (((0 0.166667) (1 0.833333) 1))))
             (((0 1) (1 0) 0))))
           (((0 0.2) (1 0.8) 1)))))))))
    (((0 0.15) (1 0.85) 1)))))))

(defvar postlex_mrpa_r_cart_tree
'((name is r)
   ((R:Segment.n.ph_vc is -)
    ((delete))
    ((nil)))
   ((nil)))
"postlex_mrpa_r_cart_tree
For remove final R when not between vowels.")

(define (postlex_apos_s_check utt)
  "(postlex_apos_s_check UTT)
Deal with possesive s for English (American and British).  Delete
schwa of 's if previous is not a fricative or affricative, and
change voiced to unvoiced s if previous is not voiced."
  (mapcar
   (lambda (seg)
     (if (string-equal "'s" (item.feat 
			     seg "R:SylStructure.parent.parent.name"))
	 (if (string-equal "a" (item.feat seg 'ph_vlng))
	     (if (and (member_string (item.feat seg 'p.ph_ctype) 
				     '(f a))
		      (not (member_string
			    (item.feat seg "p.ph_cplace") 
			    '(d b g))))
		 t;; don't delete schwa
		 (item.delete seg))
	     (if (string-equal "-" (item.feat seg "p.ph_cvox"))
		 (item.set_name seg "s")))));; from "z"
   (utt.relation.items utt 'Segment))
  utt)

(provide 'postlex)
