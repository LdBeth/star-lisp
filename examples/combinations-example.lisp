;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *lisp; -*-

(in-package :*lisp)

;;; This program can be found in /cm/starlisp/interpreter/f5301/combinations-example.lisp

;;; We define unique pariwise combinations of a set 
;;; as any two elements of the set, such that the
;;; order is not important and the element cannot be
;;; repeated.

;;; Example:  Given the set (A B C), (A B) (A C) (B C)
;;; are all of the combinations. (B A) is equivalent
;;; to (A B) and (A A) is illegal.

(defun NUMBER-OF-PAIR-COMBINATIONS (n)
  (/ (* n (1- n)) 2))

;;; This routine produces all the unique pairwise 
;;; combinations of a given set of elements.  
;;; The elements reside in the first N processors
;;; of the pvar ITEM!!, which belongs to a Vp Set
;;; which we will refer to as the 'input vp set'.

;;; The combinations that are produced end up in
;;; the first (/ (* N (- N 1)) 2) processors of
;;; the pvars FIRST-RESULT!! and SECOND-RESULT!!,
;;; which belong to a Vp Set which we will refer
;;; to as the 'output vp set'.   Note the the
;;; input and output vp sets can be the same, but
;;; need not be, nor are they necessarily the same
;;; size.

;;; Consider the five element set (A B C D E) of characters.
;;; The pvar ITEM!! would then look like:

;;; A B C D E

;;; in the first five processors.

;;; Once *PAIR-COMBINATIONS has been called, first-result!! and
;;; second-result!! will look like

;;; A A A A B B B C C D
;;; B C D E C D E D E E

;;; in the first 10 processors.

;;; The function itself returns 10, the number of processors
;;; used for the pairs.

;;; This function illustrates some uses of SENDS, GETS and SCANS,
;;; some of the basic communications primitives of the Connection
;;; Machine.


(*defun 
 *PAIR-COMBINATIONS (item!! number-of-items first-result!! second-result!!)
 
 (progn
 ;;(with-compile-time-local-property (compile-time-prop *compilep* nil)
   
   ;; Make sure that we are in the output vp set.
   
   (*with-vp-set (pvar-vp-set first-result!!)
                 
                 ;; error checking.  Make sure all the combinations will fit,
                 ;; and make sure the pvars provided for us are in the right
                 ;; Vp Sets.
                 
                 (let ((number-of-pairs (number-of-pair-combinations number-of-items)))
                   
                   (when (> number-of-pairs *number-of-processors-limit*)
                     (error "Sorry, number of pairs (~D) to be generated does not fit~@
              into the destination Vp Set, which only has ~D processors."
                       number-of-pairs *number-of-processors-limit*
                       ))
                   (when (not (eq (pvar-vp-set first-result!!) (pvar-vp-set second-result!!)))
                     (error "The two destination pvars for *PAIR-COMBINATIONS, ~S and ~S, are not in the same Vp Set!"
                       first-result!! second-result!!
                       ))
                   
                   ;; Select all the processors of the output Vp Set,
                   ;; since we have not specified what the currently
                   ;; selected set of the output vp set must be when
                   ;; we enter this routine.  (e.g., no processors
                   ;; might just happen to be selected).
                   
                   (*all
                    
                    ;; Now restrict the selected set to exactly those
                    ;; processors which will end up holding combinations.
                    
                    (*when (<!! (self-address!!) number-of-pairs)
                           
                           ;; These temporary pvars are allocated in the output vp set.
                           
                           (*let ((second-result-addr!! 1)
                                  (segment-start-p!! nil!!)
                                  )
                                 
                                 ;; Select the input vp set, and activate all its processors,
                                 ;; then subselect only those which have set elements in
                                 ;; them.  This is again because we don't know the state
                                 ;; of the selected set when we enter this routine.
                                 ;; This ensures that EXACTLY those processors we want
                                 ;; selected will end up selected.
                                 
                                 (*with-vp-set (pvar-vp-set item!!)
                                               (*all
                                                (*when (<!! (self-address!!) number-of-items)
                                                       
                                                       (*let ((previous-segment-size!! (scan!! 1 '+!! :direction :backward)))
                                                             (*setf (pref previous-segment-size!! 0) 0)
                                                             
                                                             ;; previous-segment-size!! now looks like:
                                                             ;; 0 4 3 2 1
                                                             
                                                             (*when (<!! (self-address!!) (1- number-of-items))	; don't spread last item
                                                                    (*let ((segment-addr!! (scan!! previous-segment-size!! '+!! :direction :forward)))
                                                                          
                                                                          ;; segment-addr!! now looks like:
                                                                          ;; 0 4 7 9 
                                                                          
                                                                          ;; We now send the nth element of ITEM!! to the
                                                                          ;; processor of first-result!! in the output vp set pointed at
                                                                          ;; by the nth element of segment-addr!!.
                                                                          
                                                                          ;; We use the :notify option of *PSET to inform us
                                                                          ;; of exactly which processors received values.
                                                                          
                                                                          (*pset :no-collisions item!! first-result!! segment-addr!! :notify segment-start-p!!)
                                                                          
                                                                          ;; first-result!! now looks like:
                                                                          ;; A * * * B * * C * D
                                                                          ;; where * represents an, as yet, undefined value.
                                                                          ;; segment-start-p!!, the target of our :notify, now looks like:
                                                                          ;; T NIL NIL NIL T NIL NIL T NIL T NIL NIL NIL ...
                                                                          
                                                                          (*pset :no-collisions (1+!! (self-address!!)) second-result-addr!! segment-addr!!)
                                                                          
                                                                          ;; second-result-addr!! now looks like:
                                                                          ;; 1 1 1 1 2 1 1 3 1 4
                                                                          ;; (note that when we defined second-result-addr!!
                                                                          ;; we filled it with 1 everywhere).
                                                                          
                                                                          ))))))
                                 
                                 ;; We exit the input vp set and reenter the context
                                 ;; of the output vp set, where we have put values
                                 ;; into the pvars first-result!!, segment-start-p!!
                                 ;; and second-result-addr!! by doing *PSET's across
                                 ;; Vp Sets.
                                 
                                 ;; We now use a segmented copy-scan to fill in the as yet undefined
                                 ;; values of first-result!!.
                                 
                                 (*setf first-result!! (scan!! first-result!! 'copy!! :segment-pvar segment-start-p!!))
                                 
                                 ;; first-result!! now looks like:
                                 ;; A A A A B B B C C D
                                 
                                 (*setf second-result-addr!! (scan!! second-result-addr!! '+!! :segment-pvar segment-start-p!!))
                                 
                                 ;; second-result-addr!! now looks like:
                                 ;; 1 2 3 4 2 3 4 3 4 4
                                 ;; this is the processor number of the element
                                 ;; in ITEM!! back in the input vp set which
                                 ;; we will pair with first-result!!.
                                 
                                 ;; Now we go and get that element, doing an
                                 ;; across Vp Set PREF!!.
                                 
                                 (*setf second-result!! (pref!! item!! second-result-addr!!))
                                 
                                 ;; Second-result!! looks like:
                                 ;; B C D E C D E D E E
                                 
                                 ;; So we end up with
                                 ;; first-result!!:  A A A A B B B C C D
                                 ;; second-result!!: B C D E C D E D E E
                                 
                                 ;; Yeah!
                                 
                                 )))
                   
                   number-of-pairs
                   
                   ))))



(defun test-pair-combinations ()
  (let-vp-set (input-vp-set (create-vp-set (list (max 32 *minimum-size-for-vp-set*))))
    (let-vp-set (output-vp-set (create-vp-set (list (max 64 (* 2 *minimum-size-for-vp-set*)))))
      (*with-vp-set input-vp-set
	(*let ((item!!
		 (code-char!!
		   (min!! (+!! (self-address!!) (!! (char-code #\A)))
			  (!! (+ (char-code #\A) 26))
			  ))))
	  (pppdbg item!! :end 6)
	  (*with-vp-set output-vp-set
	    (*let (first-result!! second-result!!)
	      (let ((number-of-pairs (*pair-combinations item!! 6 first-result!! second-result!!)))
		(pppdbg first-result!! :end number-of-pairs)
		(pppdbg second-result!! :end number-of-pairs)
		))))))))


#|

(test-pair-combinations)

ITEM!!: #\A #\B #\C #\D #\E #\F 
FIRST-RESULT!!: #\A #\A #\A #\A #\A #\B #\B #\B #\B #\C #\C #\C #\D #\D #\E 
SECOND-RESULT!!: #\B #\C #\D #\E #\F #\C #\D #\E #\F #\D #\E #\F #\E #\F #\F 
NIL

|#


