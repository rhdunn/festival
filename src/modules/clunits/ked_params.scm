;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                         Copyright (c) 1998                            ;;
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
;;;  ked timit data base
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Acoustic cost, features and training params
;;;

(require_module 'clunits)

(set! test_name "testx")

(set! ked_dt_params
      '(
       (name ked_timit)
       (index_name ked_timit)
;       (db_dir "/home/awb/data/timit/ked/")
       (db_dir "/data/cstrtimit/ked/")
       (disttabs_dir "festival/disttabs/")
       (utts_dir "festival/utts/")
       (utts_ext ".utt")
       (dur_pen_weight 0.5)
       (get_stds_per_unit t)
       (ac_left_context 0.8)

;       ;; fixed frame (F0 plus melcep) coefficients
;       (coeffs_dir "festival/coeffs/")
;       (coeffs_ext ".dcoeffs")
;       (ac_weights
;         (10.0
;	   0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ; 0.0
;	   2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 ; 0.0
;	   ))
;       (join_weights
;         (10.0
;	   1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 ; 0.0
;	   2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 ; 0.0
;	   ))

       ;; pitch synchronous LPC as coefficients
       (coeffs_dir "lpc/")
       (coeffs_ext ".lpc")
       (ac_weights
         ( 5.0 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
	   ))
       (join_weights
         ( 1.0 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
	   ))

       ;; Features for extraction
       (feats_dir "festival/feats/")
       (feats 
	     (occurid
	       p.name p.ph_vc p.ph_ctype 
                   p.ph_vheight p.ph_vlng 
                   p.ph_vfront  p.ph_vrnd 
		   p.ph_cplace  p.ph_cvox    
               n.name n.ph_vc n.ph_ctype 
	           n.ph_vheight n.ph_vlng 
		   n.ph_vfront  n.ph_vrnd 
                   n.ph_cplace  n.ph_cvox
	      segment_duration 
              seg_pitch p.seg_pitch n.seg_pitch
              R:SylStructure.parent.stress 
	      seg_onsetcoda n.seg_onsetcoda p.seg_onsetcoda
	      R:SylStructure.parent.accented 
	      pos_in_syl 
              syl_initial
              syl_final
	      R:SylStructure.parent.syl_break 
	      R:SylStructure.parent.R:Syllable.p.syl_break
	      pp.name pp.ph_vc pp.ph_ctype 
                  pp.ph_vheight pp.ph_vlng 
                  pp.ph_vfront  pp.ph_vrnd 
		  pp.ph_cplace pp.ph_cvox
		  ))
       ;; Wagon tree building params
       (trees_dir "festival/trees/")
       (wagon_field_desc "festival/clunits/all.desc")
       (wagon_progname "/home/awb/projects/speech_tools/bin/wagon")
       (wagon_cluster_size 20)
       (prune_reduce 0)
       ;; The dictionary of units used at run time
       (catalogue_dir "festival/clunits/")
       ;;  Run time parameters 
       (continuity_weight 10)
       (optimal_coupling 1)
       (extend_selections 1)
       (pm_coeffs_dir "pm/")
       (pm_coeffs_ext ".pm")
       (sig_dir "wav/")
       (sig_ext ".wav")
       (join_method windowed)  ;; simple, or modified_lpc
;       (pm_coeffs_dir "lpc/")
;       (pm_coeffs_ext ".lpc")
;       (sig_dir "lpc/")
;       (sig_ext ".res")
       ;; Files in db
       (files
(

"kdt_001" "kdt_002" "kdt_003" "kdt_004" "kdt_005" "kdt_006" "kdt_007"
"kdt_008" "kdt_009" "kdt_010" "kdt_011" "kdt_012" "kdt_013" "kdt_014"
"kdt_015" "kdt_016" "kdt_017" "kdt_018" "kdt_019" "kdt_020" "kdt_021"
"kdt_022" "kdt_023" "kdt_024" "kdt_025" "kdt_026" "kdt_027" "kdt_028"
"kdt_029" "kdt_030" "kdt_031" "kdt_032" "kdt_033" "kdt_034" "kdt_035"
"kdt_036" "kdt_037" "kdt_038" "kdt_039" "kdt_040" "kdt_041" "kdt_042"
"kdt_043" "kdt_044" "kdt_045" "kdt_046" "kdt_047" "kdt_048" "kdt_049"
"kdt_050" "kdt_051" "kdt_052" "kdt_053" "kdt_054" "kdt_055" "kdt_056"
"kdt_057" "kdt_058" "kdt_059" "kdt_060" "kdt_061" "kdt_062" "kdt_063"
"kdt_064" "kdt_065" "kdt_066" "kdt_067" "kdt_068"           "kdt_070"
"kdt_071" "kdt_072" "kdt_073" "kdt_074" "kdt_075" "kdt_076" "kdt_077"
"kdt_078" "kdt_079" "kdt_080" "kdt_081" "kdt_082" "kdt_083" "kdt_084"
"kdt_085" "kdt_086" "kdt_087" "kdt_088" "kdt_089" "kdt_090" "kdt_091"
"kdt_092" "kdt_093" "kdt_094" "kdt_095" "kdt_096" "kdt_097" "kdt_098"
"kdt_099" "kdt_100" "kdt_101" "kdt_102" "kdt_103" "kdt_104" "kdt_105"
"kdt_106" "kdt_107" "kdt_108" "kdt_109" "kdt_110" "kdt_111" "kdt_112"
"kdt_113" "kdt_114" "kdt_115" "kdt_116" "kdt_117" "kdt_118" "kdt_119"
"kdt_120"           "kdt_122" "kdt_123" "kdt_124" "kdt_125" "kdt_126"
"kdt_127" "kdt_128" "kdt_129" "kdt_130" "kdt_131" "kdt_132" "kdt_133"
"kdt_134" "kdt_135" "kdt_136" "kdt_137" "kdt_138" "kdt_139" "kdt_140"
"kdt_141" "kdt_142" "kdt_143" "kdt_144" "kdt_145" "kdt_146" "kdt_147"
"kdt_148" "kdt_149" "kdt_150" "kdt_151" "kdt_152" "kdt_153" "kdt_154"
"kdt_155" "kdt_156" "kdt_157" "kdt_158" "kdt_159" "kdt_160" "kdt_161"
"kdt_162" "kdt_163" "kdt_164" "kdt_165" "kdt_166" "kdt_167" "kdt_168"
"kdt_169" "kdt_170" "kdt_171" "kdt_172" "kdt_173" "kdt_174" "kdt_175"
"kdt_176" "kdt_177" "kdt_178" "kdt_179" "kdt_180" "kdt_181" "kdt_182"
"kdt_183" "kdt_184" "kdt_185" "kdt_186" "kdt_187" "kdt_188" "kdt_189"
"kdt_190" "kdt_191" "kdt_192" "kdt_193" "kdt_194" "kdt_195" "kdt_196"
"kdt_197" "kdt_198" "kdt_199" "kdt_200" "kdt_201" "kdt_202" "kdt_203"
"kdt_204" "kdt_205" "kdt_206" "kdt_207" "kdt_208" "kdt_209" "kdt_210"
"kdt_211" "kdt_212" "kdt_213" "kdt_214" "kdt_215" "kdt_216" "kdt_217"
"kdt_218" "kdt_219" "kdt_220" "kdt_221" "kdt_222" "kdt_223" "kdt_224"
"kdt_225" "kdt_226" "kdt_227" "kdt_228" "kdt_229" "kdt_230" "kdt_231"
"kdt_232" "kdt_233" "kdt_234" "kdt_235" "kdt_236" "kdt_237" "kdt_238"
"kdt_239" "kdt_240" "kdt_241" "kdt_242" "kdt_243" "kdt_244" "kdt_245"
"kdt_246" "kdt_247" "kdt_248" "kdt_249" "kdt_250" "kdt_251" "kdt_252"
"kdt_253" "kdt_254" "kdt_255" "kdt_256" "kdt_257" "kdt_258" "kdt_259"
"kdt_260" "kdt_261" "kdt_262" "kdt_263" "kdt_264" "kdt_265" "kdt_266"
"kdt_267" "kdt_268"           "kdt_270" "kdt_271" "kdt_272" "kdt_273"
"kdt_274" "kdt_275" "kdt_276" "kdt_277" "kdt_278" "kdt_279" "kdt_280"
"kdt_281" "kdt_282" "kdt_283" "kdt_284" "kdt_285" "kdt_286" "kdt_287"
"kdt_288" "kdt_289" "kdt_290" "kdt_291" "kdt_292" "kdt_293" "kdt_294"
"kdt_295" "kdt_296" "kdt_297" "kdt_298" "kdt_299" "kdt_300" "kdt_301"
"kdt_302" "kdt_303" "kdt_304" "kdt_305" "kdt_306" "kdt_307" "kdt_308"
"kdt_309" "kdt_310" "kdt_311" "kdt_312" "kdt_313" "kdt_314" "kdt_315"
"kdt_316" "kdt_317" "kdt_318" "kdt_319" "kdt_320" "kdt_321" "kdt_322"
"kdt_323" "kdt_324" "kdt_325" "kdt_326" "kdt_327" "kdt_328" "kdt_329"
"kdt_330" "kdt_331" "kdt_332" "kdt_333" "kdt_334" "kdt_335" "kdt_336"
"kdt_337" "kdt_338" "kdt_339" "kdt_340" "kdt_341" "kdt_342" "kdt_343"
"kdt_344" "kdt_345" "kdt_346" "kdt_347" "kdt_348" "kdt_349" "kdt_350"
"kdt_351" "kdt_352" "kdt_353" "kdt_354" "kdt_355" "kdt_356" "kdt_357"
"kdt_358" "kdt_359" "kdt_360" "kdt_361" "kdt_362" "kdt_363" "kdt_364"
"kdt_365" "kdt_366" "kdt_367" "kdt_368" "kdt_369" "kdt_370" "kdt_371"
"kdt_372" "kdt_373" "kdt_374" "kdt_375" "kdt_376" "kdt_377" "kdt_378"
"kdt_379" "kdt_380" "kdt_381" "kdt_382" "kdt_383" "kdt_384" "kdt_385"
"kdt_386" "kdt_387" "kdt_388" "kdt_389" "kdt_390" "kdt_391" "kdt_392"
"kdt_393" "kdt_394" "kdt_395" "kdt_396" "kdt_397" "kdt_398" "kdt_399"
"kdt_400" "kdt_401" "kdt_402" "kdt_403" "kdt_404" "kdt_405" "kdt_406"
"kdt_407" "kdt_408" "kdt_409" "kdt_410" "kdt_412" "kdt_413" "kdt_414"
"kdt_415" "kdt_416" "kdt_417" "kdt_418" "kdt_419" "kdt_420" "kdt_421"
"kdt_422" "kdt_423" "kdt_424" "kdt_425" "kdt_426" "kdt_427" "kdt_429"
"kdt_430" "kdt_431" "kdt_432" "kdt_433" "kdt_434" "kdt_435" "kdt_436"
"kdt_437" "kdt_438" "kdt_439" "kdt_440" "kdt_441" "kdt_442" "kdt_443"
"kdt_444" "kdt_445" "kdt_446" "kdt_447" "kdt_448" "kdt_449" "kdt_450"
"kdt_451"))

;       (list 'files
;	     '("kdt_001" "kdt_002" "kdt_003" "kdt_004" "kdt_005"))
       
       ))

;;; Kurt dependent set up -- fake a voice for the time being
(voice_ked_diphone)
(require 'radio_phones)
(Parameter.set 'PhoneSet 'radio)
(PhoneSet.select 'radio)
(set! clunits_params ked_dt_params)

(define (voice_ked_cluster)
  (voice_ked_diphone)
  (set! clunits_params ked_dt_params)
  (clunits:load_db clunits_params)
  (if (not (boundp 'clunit_selection_trees))
      (load (string-append
	     (string-append 
	      (get_param 'db_dir clunits_params "./")
	      (get_param 'trees_dir clunits_params "trees/")
	      (get_param 'index_name clunits_params "all")
	      ".tree"))))
  (Parameter.set 'Synth_Method 'Cluster)

  (set! current-voice 'ked_cluster)
)

(provide 'ked_params)
