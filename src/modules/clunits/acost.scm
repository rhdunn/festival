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
;;;  Finding features and acoustic distance measture for a set of 
;;;  segments in a database of utterances
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  This is primarily implement for the cluster unit selection method
;;;  but may uses in other unit selection schemes.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  There are five stages to this
;;;     Load in all utterances
;;;     Load in their coefficients
;;;     Collect together the units of the same type
;;;     build distance tables from them
;;;     dump features for them
;;;

(require_module 'clunits)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do_all)
  (let ((utterances))

    (format t "Loading utterances and sorting types\n")
    (set! utterances (acost:db_utts_load clunits_params))
    (set! unittypes (acost:find_same_types utterances))
    (acost:name_units unittypes)

    (format t "Dumping features for clustering\n")
    (acost:dump_features unittypes utterances clunits_params)

    (format t "Loading coefficients\n")
    (acost:utts_load_coeffs utterances)
    ;; If you are short of diskspace try this
    (acost:disttabs_and_clusters unittypes clunits_params)

    ;; or if you have lots of diskspace try
;    (format t "Building distance tables\n")
;    (acost:build_disttabs unittypes clunits_params)

;    ;; Build the cluster trees (requires disttabs and features)
;    (format t "Building cluster trees\n")
;    (acost:find_clusters (mapcar car unittypes) clunits_params)

    ;; Tidy up and put things together
    (acost:collect_trees (mapcar car unittypes) clunits_params)
    
    (format t "Saving unit catalogue\n")
    (acost:save_catalogue utterances clunits_params)
    
  )
)

(define (do_init)
    (set! utterances (acost:db_utts_load clunits_params))
    (set! unittypes (acost:find_same_types utterances))
    (acost:name_units unittypes)
    t)

(define (acost:disttabs_and_clusters unittypes clunits_params)
  "(acost:disttabs_and_custers unittypes)
Cause it uses so much diskspace, build each table individually
and them the cluster, removing the table before moving on to the
next."
  (mapcar
   (lambda (uu)
     (acost:build_disttabs (list uu) clunits_params)
     (acost:find_clusters (list (car uu)) clunits_params)
     (delete-file 
      (format nil "%s/%s/%s%s" 
	     (get_param 'db_dir clunits_params "./")
	     (get_param 'disttabs_dir clunits_params "disttabs/")
	     (car uu)
	     (get_param 'disttabs_ext clunits_params ".disttab")))
     )
   unittypes)
  t)

(define (acost:db_utts_load params)
  "(acost:db_utts_load params)
Load in all utterances identified in database."
  (let ((files (car (cdr (assoc 'files params)))))
    (set! acost:all_utts
	  (mapcar
	   (lambda (fname)
	     (set! utt_seg (Utterance Text fname))
	     (utt.load utt_seg 
		       (string-append 
			(get_param 'db_dir params "./")
			(get_param 'utts_dir params "utts/")
			fname
			(get_param 'utts_ext params ".utt")))
	     utt_seg)
	   files))))

(define (acost:utts_load_coeffs utterances)
  "(acost:utts_load_coeffs utterances)
Loading the acoustic coefficients of for each utterance."
  (mapcar 
   (lambda (utt) (acost:utt.load_coeffs utt clunits_params))
   utterances)
  t)

(define (acost:find_same_types utterances)
  "(acost:find_same_types utterances)
Find all the stream items of the same type and collect them into
lists of that type."
  (set! acost:unittypes nil)
  (mapcar 
   (lambda (u)
     (mapcar 
      (lambda (s) 
	(let ((p (assoc (item.name s) acost:unittypes)))
	  (if p
	      (set-cdr! p (cons s (cdr p)))
	      (set! acost:unittypes
		    (cons
		     (list (item.name s) s) acost:unittypes)))))
      (utt.relation.items u 'Segment)))
   utterances)
  acost:unittypes)

(define (acost:name_units unittypes)
  "(acost:name_units unittypes)
Names each unit with a unique id and number the occurences of each type."
  (let ((idnum 0) (tynum 0))
    (mapcar
     (lambda (s)
       (set! tynum 0)
       (mapcar
	(lambda (si)
	  (item.set_feat si "unitid" idnum)
	  (set! idnum (+ 1 idnum))
	  (item.set_feat si "occurid" tynum)
	  (set! tynum (+ 1 tynum)))
	(cdr s))
       (format t "units \"%s\" %d\n" (car s) tynum))
     unittypes)
    (format t "total units %d\n" idnum)
    idnum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating feature files

(define (acost:dump_features unittypes utterances params)
  "(acost:dump_features unittypes utterances params)
Do multiple passes over the utterances for each unittype and
dump the desired features.  This would be easier if utterances
weren't require for feature functions."
  (mapcar
   (lambda (utype)
     (acost:dump_features_utype 
      (car utype)
      utterances
      params))
   unittypes)
  t)

(define (acost:dump_features_utype utype utterances params)
  "(acost:dump_features_utype utype utterances params)
Dump features for all items of type utype."
  (let ((fd (fopen 
	     (string-append 
	      (get_param 'db_dir params "./")
	      (get_param 'feats_dir params "feats/")
	      utype
	      (get_param 'feats_ext params ".feats"))
	     "w"))
	(feats (car (cdr (assoc 'feats params)))))
    (format t "Dumping features for %s\n" utype)
    (mapcar 
     (lambda (u)
       (mapcar
	(lambda (s)
	  (if (string-equal utype (item.name s))
	      (begin 
		(mapcar 
		 (lambda (f)
		   (format fd "%s " (item.feat s f)))
		 feats)
		(format fd "\n"))))
	(utt.relation.items u 'Segment)))
     utterances)
    (fclose fd)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tree building functions

(defvar wagon-balance-size 0)

(define (acost:find_clusters unittypes clunits_params)
"Use wagon to find the best clusters."
  (mapcar
   (lambda (unittype)
     (build_tree unittype clunits_params))
   unittypes)
  t)

(define (build_tree unittype clunits_params)
"Build tree with Wagon for this unittype."
  (let ((command 
	 (format nil "%s -desc %s -data %s -balance %s -distmatrix %s -stop %s -output %s %s"
		 (get_param 'wagon_progname clunits_params "wagon")
		 (string-append
		  (get_param 'db_dir clunits_params "./")
		  (get_param 'wagon_field_desc clunits_params "wagon"))
		 (string-append 
		  (get_param 'db_dir clunits_params "./")
		  (get_param 'feats_dir clunits_params "feats/")
		  unittype
		  (get_param 'feats_ext clunits_params ".feats"))
		 (get_param 'wagon_balance_size clunits_params 0)
		 (string-append 
		  (get_param 'db_dir clunits_params "./")
		  (get_param 'disttabs_dir clunits_params "disttabs/")
		  unittype
		  (get_param 'disttabs_ext clunits_params ".disttab"))
		 (get_param 'wagon_cluster_size clunits_params 10)
		 (string-append 
		  (get_param 'db_dir clunits_params "./")
		  (get_param 'trees_dir clunits_params "trees/")
		  unittype
		  (get_param 'trees_ext clunits_params ".tree"))
		 (get_param 'wagon_other_params clunits_params "")
		 )))
    (format t "%s\n" command)
    (system command)))

(define (acost:collect_trees unittypes params)
"Collect the trees into one file as an assoc list"
  (let ((fd (fopen 
	     (string-append 
	      (get_param 'db_dir params "./")
	      (get_param 'trees_dir params "trees/")
	      (get_param 'index_name params "all.")
	      (get_param 'trees_ext params ".tree"))
	      "wb"))
	(tree_pref
	     (string-append 
	      (get_param 'db_dir params "./")
	      (get_param 'trees_dir params "trees/")))
	(cluster_prune_limit (get_param 'cluster_prune_limit params 0)))
    (format fd ";; Autogenerated list of selection trees\n")
    (mapcar
     (lambda (fp)
       (format fd ";; %l %l\n" (car fp) (car (cdr fp))))
     params)
    (format fd "(set! clunits_selection_trees '(\n")
    (mapcar
     (lambda (unit)
       (set! tree (car (load (string-append tree_pref unit ".tree") t)))
       (if (> cluster_prune_limit 0)
	   (set! tree (cluster_tree_prune tree cluster_prune_limit)))
       (pprintf (list unit tree) fd))
     unittypes)
    (format fd "))\n")
    (fclose fd)))

(define (cluster_tree_prune_in_line prune_limit)
"(cluster_tree_prune_in_line)
Prune number of units in each cluster in each tree *by* prune_limit,
if negative, or *to* prune_limit, if positive."
  (set! sucs_select_trees 
        (mapcar
	 (lambda (t)
	     (cluster_tree_prune t prune_limit))
	 sucs_select_trees)))

(define (cluster_tree_prune tree prune_limit)
"(cluster_tree_prune TREE PRUNE_LIMIT)
Reduce the number of elements in the (CART) tree leaves to PRUNE_LIMIT
removing the ones further from the cluster centre.  Maybe later this should
have guards on minimum number of units that must remain in the tree and
a per unit type limit."
  (cond
   ((cdr tree)  ;; a question
    (list
     (car tree)
     (cluster_tree_prune (car (cdr tree)) prune_limit)
     (cluster_tree_prune (car (cdr (cdr tree))) prune_limit)))
   (t           ;; tree leave
    (list 
     (list
      (remove_n_worst 
       (car (car tree))
       (if (< prune_limit 0)
	   (* -1 prune_limit)
	   (- (length (car (car tree))) prune_limit)))
      (car (cdr (car tree))))))))

(define (remove_n_worst lll togo)
"(remove_n_worst lll togo)
Remove togo worst items from lll."
  (cond
   ((< togo 0)
    lll)
   ((equal? 0 togo)
    lll)
   (t
    (remove_n_worst
     (remove (worst_unit (cdr lll) (car lll)) lll)
     (- togo 1)))))

(define (worst_unit lll worst_so_far)
"(worst_unit lll worst_so_far)
Returns unit with worst score in list."
  (cond
   ((null lll)
    worst_so_far)
   ((< (car (cdr worst_so_far)) (car (cdr (car lll))))
    (worst_unit (cdr lll) (car lll)))
   (t
    (worst_unit (cdr lll) worst_so_far))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save the unit catalogue for use in the run-time index

(define (acost:save_catalogue utterances clunits_params)
  "(acost:save_catalogue utterances clunits_params)
Save the catalogue with named units with times."
 (let ((fd (fopen 
	    (string-append 
	     (get_param 'db_dir clunits_params "./")
	     (get_param 'catalogue_dir clunits_params "trees/")
	     (get_param 'index_name clunits_params "catalogue.")
	     ".catalogue")
	      "wb"))
       (num_units 0)
       )
   (format fd "EST_File index\n")
   (format fd "DataType ascii\n")
   (format fd "NumEntries %d\n"
	   (apply 
	    + (mapcar (lambda (u) 
			(length (utt.relation.items u 'Segment))) utterances)))
   (format fd "IndexName %s\n" (get_param 'index_name clunits_params "cluser"))
   (format fd "EST_Header_End\n")
   (mapcar
    (lambda (u)
      (mapcar
       (lambda (s)
	 (format fd "%s_%s %s %f %f %f\n"
		 (item.name s)
		 (item.feat s 'occurid)
		 (utt.feat u 'fileid)
		 (item.feat s 'segment_start)
		 (item.feat s 'segment_mid)
		 (item.feat s 'segment_end)))
       (utt.relation.items u 'Segment)))
    utterances)
   (fclose fd)))

(defSynthType Cluster
  (let ((join_method (get_param 'join_method clunits_params 'simple)))
    (format t "Cluster synth\n")
    (Clunits_Select utt)
    (Clunits_Get_Units utt)
    ;; Choice of function to put them together
    (cond
     ((string-equal join_method 'windowed)
      (Clunits_Windowed_Wave utt))
     ((string-equal join_method 'none)
      t)
     ((string-equal join_method 'modified_lpc)
      (defvar UniSyn_module_hooks nil)
      (Param.def "unisyn.window_name" "hanning")
      (Param.def "unisyn.window_factor" 1.0)
      (Parameter.def 'us_sigpr 'lpc)
      (mapcar 
       (lambda (u s)
	 (item.set_feat s "source_end" (item.feat u "end")))
       (utt.relation.items utt 'Unit)
       (utt.relation.items utt 'Segment))
      (us_unit_concat utt)
      (if (not (member 'f0 (utt.relationnames utt)))
	  (targets_to_f0 utt))
      (if (utt.relation.last utt 'Segment)
	  (set! pm_end (+ (item.feat (utt.relation.last utt 'Segment) "end")
			  0.02))
	  (set! pm_end 0.02))
      (us_f0_to_pitchmarks  utt 'f0 'TargetCoef pm_end)
      (us_mapping utt 'segment_single)
      (us_generate_wave utt (Parameter.get 'us_sigpr)
			'analysis_period))
     (t
      (Clunits_Simple_Wave utt)))
    utt
  )
)

;;;
;;;  Some test sentences
;;;
(defvar test_name "test1")

(define (do_cluster_test)
  (let ((dddir "/usr/awb/projects/festival/src/modules/clunits/tests/"))
    (system (format nil "mkdir %s%s" dddir test_name))
    (set! utt1 (Utterance Text "hello"))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test1.wav"))
    (set! utt1 (Utterance Text "She had your dark suit in greasy washwater all year."))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test2.wav"))
    (set! utt1 (Utterance Text "The smell of the freshly groun coffee, never fails to entice me into the shop."))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test3.wav"))
    (set! utt1 (Utterance Text "The smell of the freshly ground coffee, never fails to entice me into the shop."))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test3.wav"))
    (set! utt1 (Utterance Text "This is a short introduction to the Festival Speech Synthesis system."))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test4.wav"))
    (set! utt1 (Utterance Text "My telephone number is 650-2787."))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test5.wav"))))

(define (do_cluster_test2)
  (let ((dddir "/usr/awb/projects/festival/src/modules/clunits/tests/"))
    (system (format nil "mkdir %s%s" dddir test_name))
    (set! utt1 (Utterance Text "U.S. President Bill Clinton on Tuesday, ratcheted up the pressure on Yugoslavia"))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test6.wav"))
    (set! utt1 (Utterance Text "They wanted to go on a barge trip."))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test7.wav"))
    (set! utt1 (Utterance Text "The weather in Pittsburgh tomorrow will be fine, in the mid 70s."))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test8.wav"))
    (set! utt1 (Utterance Text "But revenge is all I have left."))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "test9.wav"))
    (set! utt1 (Utterance Text "Thank you for calling, please hold, while I play you some bad music"))
    (utt.save.wave (utt.synth utt1)
		   (format nil "%s%s/%s" dddir test_name "testA.wav"))
    ))

(provide 'clunits)
