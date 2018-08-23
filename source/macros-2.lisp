;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: *SIM-I -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;; ****************************************************************************
;;;;
;;;;                         CODE FOR OTHER *LISP MACROS
;;;;
;;;; ****************************************************************************



(defun remove-return-pvar-p-from-body (body function-name)
  (if (and (listp body)
	   (listp (car body))
	   (eq 'declare (caar body))
	   )
      (let ((declare-form (car body)))
	(if (return-pvar-p-declaration-p declare-form)
	    (let ((value (return-pvar-p-declaration-value declare-form)))
	      (if (member value '(t nil))
		  (values (cdr body) (if value :yes :no))
		  (error "Illegal value, ~S, for return-pvar-p declaration.  Only T or NIL are allowed" value)
		  ))
	    (error "Illegal declare form, ~S, in ~S.  Only return-pvar-p declarations are allowed." declare-form function-name)
	    ))
      (values body :maybe)
      ))


(defmacro *all (&body body)
  "Select all processors for the body"
  (multiple-value-bind (body return-pvar-p)
      (remove-return-pvar-p-from-body body '*all)
    (let ((css-index-symbol (gensym "CSS-INDEX-"))
	  (value-symbol (gensym "*ALL-RETURN-VALUE-"))
	  (old-temp-pvar-list-symbol (gensym "OLD-TEMP-PVAR-LIST-"))
	  )
      `(let ((,css-index-symbol *css-current-level*)
	     (,old-temp-pvar-list-symbol *temp-pvar-list*)
	     )
	 (prog2
	   (push-css-select-all)
	   (let ((,value-symbol (progn .,body)))
	     (check-return-pvar-p  ,value-symbol ,return-pvar-p)
	     (handle-returning-pvar ,value-symbol ,old-temp-pvar-list-symbol nil)
	     )
	   (pop-css-to-level ,css-index-symbol)
	   )))))


(defmacro *when (test-pvar &body body)
  "subselect all processors with test-pvar non-nil"
  (multiple-value-bind (body return-pvar-p)
      (remove-return-pvar-p-from-body body '*when)
    (let ((css-index-symbol (gensym "CSS-INDEX-"))
	  (old-temp-pvar-list-symbol (gensym "OLD-TEMP-PVAR-LIST-"))
	  (test-pvar-symbol (gensym "TEST-PVAR-"))
	  (value-symbol (gensym "*WHEN-RETURN-VALUE-"))
	  )
      `(let* ((,css-index-symbol *css-current-level*)
	      (,old-temp-pvar-list-symbol *temp-pvar-list*)
	      (,test-pvar-symbol ,test-pvar)
	      ,value-symbol)
	 (simple-pvar-argument!! ,test-pvar-symbol)
	 (setq ,value-symbol
	       (prog2
		 (push-css ,test-pvar-symbol)
		 (progn ,@body)
		 (pop-css-to-level ,css-index-symbol)
		 ))
	 (check-return-pvar-p ,value-symbol ,return-pvar-p)
	 (handle-returning-pvar
	   ,value-symbol
	   ,old-temp-pvar-list-symbol
	   nil
	   )))))


(defmacro *unless (test-pvar &body body)
  `(*when (not!! ,test-pvar)
     ,@body
     ))


(defmacro *if (condition true-clause &optional else-clause)
  (if (null else-clause)
      `(progn (*when ,condition ,true-clause) (values))
      `(progn
	 ,(let ((condition-symbol (gensym "*IF-CONDITION-")))
	    `(*let ((,condition-symbol ,condition))
	       (*when ,condition-symbol ,true-clause)
	       (*when (not!! ,condition-symbol) ,else-clause)
	       ))
	 (values)
	 )))


(defmacro *cond (&rest clauses)
  "similar to *if"
  (cond ((null clauses) nil)
	((eql (length clauses) 1)
	 `(*if ,(first (first clauses)) (progn ,@(rest (first clauses))))
	 )
	((eq (first (first clauses)) 't!!)
	 ;; if there are more clauses, issue a warning to that effect
	 (when (not (eql (length clauses) 1))
	   (error "~% *cond: t!! is used in a clause other than the last")
	   ))
	(t
	 `(*if ,(first (first clauses))
	       (progn ., (rest (first clauses)))
	       (*cond ., (rest clauses))))))


(defmacro if!! (pvar-expression true-pvar &optional (else-pvar nil!!))

  "IF!! will return a pvar.  This pvar will contain TRUE-PVAR
   for all processors with PVAR-EXPRESSION 
   true and ELSE-PVAR for all those with PVAR-EXPRESSION false.
   During the execution of TRUE-PVAR, the CSS
   will be set to those processors that passed PVAR-EXPRESSION,
   whereas during execution of ELSE-PVAR, the
   CSS will be set to those processors which failed PVAR-EXPRESSION.
  "
  
  (let ((if!!-result-symbol (gensym "IF!!-RESULT-")))
    `(*let (,if!!-result-symbol)
       (*if ,pvar-expression (*set ,if!!-result-symbol ,true-pvar) (*set ,if!!-result-symbol ,else-pvar))
       ,if!!-result-symbol
       )))

(defmacro cond!! (&rest clauses)
  (let ((len (length clauses)))
    (cond ( (eql 0 len) 'nil!! )
	  ( (eql 1 len)
	    (let* ((clause (car clauses))
		   (test (car clause))
		   (consequents (cdr clause))
		  )
	      (if (null consequents)
		  test
		  `(if!! ,test (progn ,@consequents) nil!!)
	       ))
	  )
	  ( t
	    (let* ((clause1 (car clauses))
		   (test1 (car clause1))
		   (consequents1 (cdr clause1))
		  )
	      (if (null consequents1)
		  (let ((test-symbol (gensym "COND!!-TEMP-TEST-")))
		    `(*let ((,test-symbol ,test1))
		       (if!! ,test-symbol
			     ,test-symbol
			     (cond!! ,@(cdr clauses))
			)))
		  `(if!! ,test1
			 (progn ,@consequents1)
			 (cond!! ,@(cdr clauses))
		    )))
	  )
      )))



(defmacro and!! (&rest pvars)
  (cond ((null pvars) 't!!)
	((= 1 (length pvars))
	 (let ((sym (gensym "and!!-")))
	   `(let ((,sym ,(first pvars)))
	      (simple-pvar-argument!! ,sym)
	      (pvar-check ,sym)
	      ,sym
	     )))
	(t
	 `(if!! ,(first pvars) (and!! ., (rest pvars)) nil!!))))

;;; (and!! 1 2)



(defmacro or!! (&rest pvars)
  (cond ((null pvars) 'nil!!)
	((= 1 (length pvars))
	 (let ((sym (gensym "or!!-")))
	   `(let ((,sym ,(first pvars)))
	      (simple-pvar-argument!! ,sym)
	      (pvar-check ,sym)
	      ,sym
	     )))
	(t
	 `(let ((temp ,(first pvars)))
	    (if!! temp temp (or!! ., (rest pvars)))))))

;;; (or!! 1 2 3)


(defmacro with-css-saved (&rest body)

  "Save the state of the temporary pvar list and the
   currently selected set.  If anyone tries to
   break out of the body of the form we use
   unwind-protect to restore state before allowing
   the exit.
  "

  (let ((foo (gensym "OLD-CSS-LEVEL"))
	(bar (gensym "OLD-TEMP-PVAR-LIST-"))
	(baz (gensym "WITH-CSS-BODY-VALUE-"))
       )
    `(let ((,foo *css-current-level*)
	   (,bar *temp-pvar-list*)
	   (,baz nil)
	  )
       (unwind-protect
	   (setq ,baz (progn ,@ body))
	 (progn
	   (pop-css-to-level ,foo)
	   (handle-returning-pvar ,baz ,bar nil)
	   )))))


(defun *defun-function-symbol-exists? (symbol)
  (let ((*defun-symbol (make-*defun-function symbol)))
    (if (and (macro-function symbol) (fboundp *defun-symbol)) *defun-symbol nil)
    ))

(defun *apply-error (who f-name)
  (error "In ~A: The ~S operator is a macro.~%~
    ~A cannot accept macros other than those defined by *DEFUN."
	 who f-name who))

(defmacro *funcall (f &rest args)
  (let ((actual-function-symbol (gensym "*FUNCALL-ACTUAL-FUNCTION-"))
	(old-temp-pvar-symbol (gensym "*FUNCALL-OLD-TEMP-PVAR-"))
	(f-symbol (gensym "FUNCTION-ARGUMENT-"))
       )
    `(let* ((,old-temp-pvar-symbol *temp-pvar-list*)
	    (,f-symbol ,f)
	    (,actual-function-symbol
	      (if (symbolp ,f-symbol)
		  (or (*defun-function-symbol-exists? ,f-symbol)
		      (when (macro-function ,f-symbol)
			(*apply-error '*funcall ,f-symbol))
		      ,f-symbol)
		  ,f-symbol
		  ))
	    )
       (handle-returning-pvar
	 (funcall ,actual-function-symbol ,@args) ,old-temp-pvar-symbol nil)
       )))


(defmacro *apply (f arg &rest args)
  (let ((actual-function-symbol (gensym "*APPLY-ACTUAL-FUNCTION-"))
	(old-temp-pvar-symbol (gensym "*APPLY-OLD-TEMP-PVAR-"))
	(f-symbol (gensym "FUNCTION-ARGUMENT-"))
	)
    `(let* ((,old-temp-pvar-symbol *temp-pvar-list*)
	    (,f-symbol ,f)
	    (,actual-function-symbol
	      (if (symbolp ,f-symbol)
		  (or (*defun-function-symbol-exists? ,f-symbol)
		      (when (macro-function ,f-symbol)
			(*apply-error '*apply ,f-symbol))
		      ,f-symbol)
		  ,f-symbol
		  ))
	    )
       (handle-returning-pvar
	 (apply ,actual-function-symbol (list* ,arg ,@args)) ,old-temp-pvar-symbol nil)
       )))


;;;; Code for CASE!! and friends.  Written by Jim Salem.


(defmacro once-only (variable-list &body body)
  (cond ((null variable-list) `(progn .,body))
	(t
	 (let* ((var (car variable-list)))
	   `(once-only ,(cdr variable-list)
	      (cond ((or (atom ,var) (member (car ,var) '(quote function)))
		     ;;; Variable is an atom or quoted.
		     ;;; Don't expand differently
		     .,body)
		    (t
		     (let* ((temp (gensym "ONCE-ONLY-VAR"))
			    (actual-arg ,var)
			    (form (gensym "ONCE-ONLY"))
			    (,var temp)
			    )
		       (setq form (progn .,body))
		       `(let ((,temp ,actual-arg))
			  ,form)))))))))


(defmacro def-*lisp-case-form (case-function-name ecase-function-name cond-function-name)

  `(progn

     (defmacro ,case-function-name (pvar-expression &body clauses)
       (once-only (pvar-expression)
	 `(,',cond-function-name
	    .,(let ((last-clause-found-p nil)
		    keys body
		    (return-cond-clauses nil)
		    )
		(dolist (clause clauses)
		  ;; For example, CLAUSE could be  ((:apples :oranges) (eat-fruit :seedsp t))
		  (setq keys (car clause)) ;; E.G. (:apples :oranges)
		  (setq body (cdr clause)) ;; E.G. ((eat-fruit :seedsp t))
		  
		  ;;; WARNINGS ---

		  ;;; Do we have a clause we can never reach ?
		  (when last-clause-found-p
		    (warn "The clause ~S appeared after a T or OTHERWISE clause in ~A"
			  clause ',case-function-name))

		  (when (or (eql keys 't!!) (and (listp keys)
						 (or (eql (car keys) '!!)
						     (and (listp (car keys))
							  (eql (caar keys) '!!))
						     )))
		    (warn "The keywords for ~A shouldn't be pvars (in clause ~S)."
			  ',case-function-name
			  clause)
		    )

		  ;;; GENERATE COND CLAUSES
		  (push (cond 
			  ((listp keys)
			   ;;; Multiple keywords for this clause
			   `((or!! .,(mapcar #'(lambda (x)
						 `(eql!! ,pvar-expression (!! ',x))) keys))
			     .,body))
			  ((or (eql keys 't) (eql keys 'otherwise))
			   ;;; Final clause
			   (setq last-clause-found-p t)
			   `(t!! .,body))
			  (t
			   ;;; A clause with a single keyword
			   `((eql!! ,pvar-expression (!! ',keys)) .,body)
			   )
			  )

			return-cond-clauses))
		(nreverse return-cond-clauses)
		))))

     (defmacro ,ecase-function-name (pvar-expression &body clauses)
       (let ((all-keys nil)
	     keys
	     )
	 ;;; Collect the names of all the keys
	 (dolist (clause clauses)
	   (setq keys  (car clause))
	   (cond ((atom keys) (push keys all-keys))
		 (t (dolist (key keys)
		      (push key all-keys)
		      ))
		 ))
	 `(,',case-function-name ,pvar-expression
	    ,@clauses
	    (t
	      (when (*or t!!)
		(error "For ~A, PVAR-EXPRESSION in some processor was not one of ~{~S~^, ~}."
		       ',',ecase-function-name ',all-keys))

	      nil!!))))
     ))

(def-*lisp-case-form case!! ecase!! cond!!)
(def-*lisp-case-form *case *ecase *cond) 



(defmacro incf-use-count (function-name)
;  `(incf (get ,function-name 'use-count))
  (declare (ignore function-name))
  nil
 )


(defmacro assocv-cadr (value list) `(cadr (assoc ,value ,list)))

(defmacro assocv-cdr (value list) `(cdr (assoc ,value ,list)))


(defmacro no-processors-active () `(*and nil!!))
(defmacro some-processor-active () `(not (*and nil!!)))
(defmacro all-processors-active () `(eql *number-of-processors-limit* (*sum (!! 1))))


(defmacro with-array-elements-iterated (array var &body body)
  (assert (symbolp var))
  (let ((array-symbol (gensym "ARRAY-"))
	(body-function (gensym "WITH-ARRAY-ELEMENTS-ITERATED-BODY-FUNCTION-"))
	)
    `(let ((,array-symbol ,array))
       (flet
	 ((,body-function (,var) ,@body))
	 (if (not (eql 1 (array-rank ,array-symbol)))
	     (let ((displaced-array (make-array (array-total-size ,array-symbol) :displaced-to ,array-symbol)))
	       (dotimes (j (length displaced-array))
		 (,body-function (aref displaced-array j))
		 ))
	     (dotimes (j (length ,array-symbol))
	       (,body-function (aref ,array-symbol j))
	       ))))))


(defmacro with-many-array-elements-iterated ((&rest vars) (&rest arrays) &body body)

  (assert (every #'symbolp vars))
  (assert (eql (length vars) (length arrays)) () "Number of variables not equal to number of arrays to iterate over")

  (let* ((array-symbols (mapcar #'(lambda (var) (gensym (concatenate 'string (symbol-name var) "-ARRAY-"))) vars))
	 (displaced-array-symbols
	   (mapcar #'(lambda (array-symbol) (gensym (concatenate 'string "DISPLACED-" (symbol-name array-symbol)))) array-symbols))
	 (body-function (gensym "WITH-MANY-ARRAY-ELEMENTS-ITERATED-BODY-FUNCTION-"))
	)

    (if (zerop (length vars))

	nil

	`(let
	   (,@(mapcar #'(lambda (array-symbol array) `(,array-symbol ,array)) array-symbols arrays))
	   (flet
	     ((,body-function (,@vars) ,@body))
	     (let* ((first-array ,(first array-symbols))
		    (array-dimensions (array-dimensions first-array))
		    )
	       (assert (every #'(lambda (array) (equal array-dimensions (array-dimensions array))) (list ,@array-symbols))
		       ()
		       "Error.  All the arrays you are iterating over do not have the same dimensions"
		       )
	       (if (eql 1 (array-rank first-array))
		   (dotimes (j (length ,(first array-symbols)))
		     (,body-function ,@(mapcar #'(lambda (array-symbol) `(aref ,array-symbol j)) array-symbols))
		     )
		   (let
		     (,@(mapcar
			  #'(lambda (displaced-array-symbol array-symbol)
			      `(,displaced-array-symbol (make-array (array-total-size ,array-symbol) :displaced-to ,array-symbol))
			      )
			  displaced-array-symbols array-symbols
			  ))
		     (dotimes (j (length ,(first displaced-array-symbols)))
		       (,body-function ,@(mapcar #'(lambda (array-symbol) `(aref ,array-symbol j)) displaced-array-symbols))
		       )))))))))


(defmacro with-structure-elements-iterated
	  (((&rest vars) (&rest structures) slot-accessor-functions &key (aliased? nil)) &body body)
  
  (assert (every #'symbolp vars))
  (assert (eql (length vars) (length structures)) () "Number of variables not equal to number of structures to iterate over")

  (let* ((structure-symbols (mapcar #'(lambda (var) (gensym (concatenate 'string (symbol-name var) "-STRUCTURE-"))) vars))
	 (body-function (gensym "WITH-STRUCTURE-ELEMENTS-ITERATED-BODY-FUNCTION-"))
	 (slot-accessor-functions-list-symbol (gensym "SLOT-ACCESSORS-LIST-"))
	)

    (if (zerop (length vars))

	nil

	`(let
	   (,@(mapcar #'(lambda (structure-symbol structure) `(,structure-symbol ,structure)) structure-symbols structures))
	   (let ((,slot-accessor-functions-list-symbol ,slot-accessor-functions))
	     (flet
	       ((,body-function (,@vars) ,@body))
	       (let* ((first-structure ,(first structure-symbols))
		      (type (type-of first-structure))
		      )
		 (assert (every #'(lambda (structure) (eq type (type-of structure))) (list ,@structure-symbols))
			 ()
			 "Error.  All the structures you are iterating over are not of the same type"
			 )
		 (dolist (slot-accessor
			   ,(if (not aliased?)
				slot-accessor-functions-list-symbol
				`(mapcar #'*defstruct-slot-alias!!-function ,slot-accessor-functions-list-symbol)
				))
		   (,body-function
		    ,@(mapcar #'(lambda (structure) `(funcall slot-accessor ,structure)) structure-symbols)
		    )))))))))


(defmacro do-for-selected-processors-internal ((var) &body body)
  (assert (symbolp var))
  (let ((local-css-symbol (gensym "LOCAL-CSS-")))
    `(let ((,local-css-symbol *css*))
       (with-bit-vectors (,local-css-symbol)
         (dotimes (,var *number-of-processors-limit*)
           (declare (type fixnum ,var))
           (when (eql 1 (sbit ,local-css-symbol ,var)) ,.body)
           )))))


(defun any-processors-active-function ()
  (block 
   exit
   (do-for-selected-processors-internal (j) (return-from exit t))
   nil
   ))

(defun all-processors-active-function ()
  (let ((count 0))
    (do-for-selected-processors-internal (j) (incf count))
    (= count *number-of-processors-limit*)
    ))

(defmacro with-selected-general-pvar-arrays ((processor-index &key (return-any-set t)) (&rest vars) (&rest pvars) &body body)
  (assert (eql (length vars) (length pvars)))
  (assert (every #'symbolp vars))
  (assert (symbolp processor-index))
  (let ((any-set-symbol (gensym "ANY-SET-")))
    `(let
         (,@(mapcar #'(lambda (symbol pvar) `(,symbol (pvar-array ,pvar))) vars pvars)
             ,@(if return-any-set `((,any-set-symbol nil)))
             )
       (with-simple-vectors (,@vars)
         (do-for-selected-processors-internal (,processor-index)
           ,@(if return-any-set `((setq ,any-set-symbol t)))
           ,@body
           )
         ,@(if return-any-set `(,any-set-symbol))
         ))))

(defmacro with-scalar-body-mapped-into-result-pvar ((result-pvar) (&rest source-pvars) function-of-n-scalars)
  (let ((result-pvar-array-symbol (gensym "RESULT-PVAR-ARRAY-"))
	(index-symbol (gensym "PROCESSOR-INDEX-"))
	)
    (let* ((source-pvar-symbols 
            (mapcar #'(lambda (pvar) (declare (ignore pvar)) (gensym "SOURCE-PVAR-ARRAY-")) source-pvars))
	   (aref-forms (mapcar #'(lambda (symbol) `(aref ,symbol ,index-symbol)) source-pvar-symbols))
	   )
      `(with-selected-general-pvar-arrays
	 (,index-symbol) (,@(cons result-pvar-array-symbol source-pvar-symbols)) (,@(cons result-pvar source-pvars))
	 (setf (aref ,result-pvar-array-symbol ,index-symbol)
	       ,(if (symbolp function-of-n-scalars)
		    `(,function-of-n-scalars ,@aref-forms)
		    `(funcall ,function-of-n-scalars ,@aref-forms)
		    ))))))

(defmacro *compile ((&key (safety 1) (warning-level :normal)) &body body)
  (declare (ignore safety warning-level))
  `(progn ,@body)
  )


(defmacro *nocompile (&body body) `(progn ,@body))

(defmacro *compile-blindly (&body body) `(progn ,@body))

(defun *lisp-compiler-hook (form env) form env nil)

(defmacro pref
    #-(OR :CCL :ALLEGRO KCL)
    (&whole form pvar processor &key vp-set &environment macroexpand-environment)
    #+(OR :CCL :ALLEGRO KCL)
    (&whole form &environment macroexpand-environment pvar processor &key vp-set)

  ;; What's going on here?

  ;; We need to look at the form to evaluate to get the pvar being referenced,
  ;; and the form to evaluate to get the processor from which we will do
  ;; the extraction.

  ;; There are two cases for the pvar expression.  Either it is a symbol or
  ;; it is not.  If it is a symbol, life is easy; we can just call
  ;; pref-function, which works with a pvar in any vp set.  If it is not
  ;; a symbol, then two things must happen:  we must evaluate the expression
  ;; in the proper vp set, and we must evaluate the expression in the
  ;; active set which consists solely of the processor being read from.
  ;; The :vp-set argument, if provided, is used to determine the proper
  ;; vp set in expression case.  If it is not provided in the expression
  ;; case the *current-vp-set* is assumed, and the user will lose if
  ;; the expression references pvars in a different vp set.

  ;; There are two major cases for the processor form.  Either it is
  ;; of the form (grid!! &rest args) or it is not.

  ;; If it is a grid!! call, then there are 3 subcases:

  ;; First, if no :vp-set argument is provided and the pvar expression
  ;; is a symbol, we convert the arguments of the grid!! call into
  ;; a cube address using the vp-set of the pvar symbol.

  ;; Second, if a :vp-set argument is provided, we convert the
  ;; arguments using that vp set.

  ;; Finally, if no :vp-set argument is provided and the pvar expression
  ;; is not a symbol we convert using *current-vp-set*.

  ;; If the form is not a grid!! call, then what it evaluates to can
  ;; be one of two things: a cube address or an address object.

  ;; If it is a cube address we just use it.  If it is an address object,
  ;; then we must extract a cube address from it.  To do this we must
  ;; transform the address object into an address object in the proper
  ;; vp-set.  The proper vp set is one of three possibilities, as discussed
  ;; above.

  ;; First check if the *compiler wants to deal with it.  If so, let it!

  (or (*lisp-compiler-hook form macroexpand-environment)     

      (let ((me-pvar-expression (macroexpand pvar macroexpand-environment))
	    (me-processor-expression (macroexpand processor macroexpand-environment))
	    )
	(let* ((is-grid-expression (and (listp me-processor-expression) (eq 'grid (car me-processor-expression))))
	       (processor-form
		 (if is-grid-expression
		     (cond
		       (vp-set `(cube-from-vp-grid-address ,vp-set ,@(cdr me-processor-expression)))
		       ((symbolp me-pvar-expression)
			`(cube-from-vp-grid-address (pvar-vp-set ,me-pvar-expression) ,@(cdr me-processor-expression))
			)
		       (t `(cube-from-grid-address ,@(cdr me-processor-expression)))
		       )
		     me-processor-expression
		     ))
	       )
		       
	  (if (symbolp me-pvar-expression)
	      (let ((vp-set-form (if vp-set vp-set `(if (fast-pvarp ,me-pvar-expression)
							(pvar-vp-set ,me-pvar-expression)
							*current-vp-set*))))
		`(new-pref-function ,me-pvar-expression ,processor-form ,vp-set-form))
	      (let ((vp-set-form (if vp-set vp-set '*current-vp-set*)))
		`(new-pref-function #'(lambda () 
					(let ((pvar-exp ,me-pvar-expression))
					  (simple-pvar-argument!! pvar-exp)
					  pvar-exp)) ,processor-form ,vp-set-form)))))))


(defmacro pref-grid (pvar &rest indices) `(pref ,pvar (grid ,@indices)))

(defmacro all!! (&body body) `(*all (declare (return-pvar-p t)) ,@body))
(defmacro when!! (condition &body body) `(*when ,condition (declare (return-pvar-p t)) ,@body))
(defmacro let!! (bindings &body body) `(*let ,bindings (declare (return-pvar-p t)) ,@body))

(defmacro with-grid-indices-iterated

	  ((iterated-index-list-symbol
	     number-of-grid-indices
	     &key
	     start-index-list
	     end-index-list
	     mask
	     (check-arguments t)
	     (direction :backward)
	     (bind-as :list)
	     )
	   &rest body
	   )

  (assert (symbolp iterated-index-list-symbol))

  (let ((start-temp (gensym "START-INDEX-LIST-"))
	(end-temp (gensym "END-INDEX-LIST-"))
	(mask-temp (gensym "MASK-"))
	(n-dimensions-temp (gensym "N-DIMENSIONS-"))
	(direction-temp (gensym "DIRECTION-"))
	(bind-as-temp (gensym "BIND-AS-"))
	)

    `(let* ((,start-temp ,start-index-list)
	    (,end-temp ,end-index-list)
	    (,mask-temp ,mask)
	    (,n-dimensions-temp ,number-of-grid-indices)
	    (,direction-temp ,direction)
	    (,bind-as-temp ,bind-as)
	    )

       (when ,check-arguments
	 (check-args-for-with-grid-indices-iterated
	   ,direction-temp ,bind-as-temp ,mask-temp ,start-temp ,end-temp ,n-dimensions-temp))

       (when (null ,start-temp)
	 (setq ,start-temp (make-sequence 'vector ,n-dimensions-temp :initial-element 0)))
       (when (null ,end-temp)
	 (setq ,end-temp (concatenate 'vector *current-cm-configuration*)))
       (when (null ,mask-temp)
	 (setq ,mask-temp (make-list ,n-dimensions-temp :initial-element t)))


       (setq ,iterated-index-list-symbol
	     (concatenate (if (eq ,bind-as-temp :list) 'list 'vector) ,start-temp))

       (loop

         (progn ,@body)

	 (when
	   (null
	     (next-grid-coordinates
	       ,start-temp
	       ,end-temp
	       ,iterated-index-list-symbol
	       ,mask-temp
	       ,n-dimensions-temp
	       ,direction-temp
	       ))
	   (return)
	   )

	 ))))



(defmacro with-displaced-arrays ((&rest arrays) (&rest displaced-array-symbols) &body body)
  (assert (eql (length arrays) (length displaced-array-symbols)))
  (assert (every #'symbolp arrays))
  `(let
     ,(mapcar
	#'(lambda (array displaced-array-symbol)
	    `(,displaced-array-symbol
	      (if (vectorp ,array)
		  ,array
		  (make-array (array-total-size ,array) :displaced-to ,array :element-type (array-element-type ,array))
		  )))
	arrays
	displaced-array-symbols
	)
     ,@body
     ))


(defmacro defun-wcefi (name args &body body)
  `(defun ,name ,args ,@body)
  )

(defmacro defun-wco (name args &body body)
  `(defun ,name ,args ,@body)
  )

(defmacro pref-grid-relative!! (pvar &rest index-pvars)
  (declare (ignore pvar index-pvars))
  (error "The macro pref-grid-relative!! is obsolete as of Release 5.0.  Use NEWS!!, NEWS-BORDER!! or PREF!! instead.")
  )

(defmacro address-object-cached-geometry-id (address-object-pvar)
  `(pvar-address-object-geometry-id ,address-object-pvar)
  )
(defmacro set-address-object-cached-geometry-id (address-object-pvar geometry-id)
  `(setf (pvar-address-object-geometry-id ,address-object-pvar) ,geometry-id)
  )

(defun cache-address-object-pvar (ao geometry-id)
  (set-address-object-cached-geometry-id ao geometry-id)
  )
(defun decache-address-object-pvar (ao)
  (set-address-object-cached-geometry-id ao nil)
  )

(defmacro coerce-integer-pvar-into-dest (dest source &key dest-type)
  (declare (ignore dest-type))
  `(*set ,dest ,source)
  )

(defmacro without-void-pvars (pvar-list &body body)
  (declare (ignore pvar-list))
  `(progn ,@body)
  )


(defmacro with-*defstruct-accessors-iterated ((accessor-function-var *defstruct-name) &body body)
  `(dolist (,accessor-function-var (list-of-*defstruct-accessors ,*defstruct-name))
     ,@body
     ))

(defmacro with-*defstruct-slot-descriptors-iterated ((slot-descriptor-var *defstruct-name) &body body)
  `(dolist (,slot-descriptor-var (list-of-*defstruct-slot-descriptors ,*defstruct-name))
     ,@body
     ))

(defun list-of-*defstruct-slot-descriptors (*defstruct-name)
  (*defstruct-all-slots-list (get *defstruct-name '*defstruct-structure))
  )

(defun list-of-*defstruct-accessors (*defstruct-name)
  (let* ((*defstruct-structure (get *defstruct-name '*defstruct-structure))
	 (slot-accessor-prefix-name (*defstruct-slot-accessor-prefix-name *defstruct-structure))
	 (slots-list (*defstruct-all-slots-list *defstruct-structure))
	 )
    (mapcar
      #'(lambda (slot)
	  (let ((slot-name (*defstruct-slot-name slot)))
	    (let ((accessor-function-name
		    (intern (concatenate
			      'string
			      (string slot-accessor-prefix-name)
			      (string slot-name)
			      "!!"
			      )
			    (symbol-package *defstruct-name)
			    )))
	      (when (not (fboundp accessor-function-name))
		(error "The accessor function name ~S for *defstruct ~S does not have a function binding"
		       accessor-function-name *defstruct-name
		       ))
	      accessor-function-name
	      )))
      slots-list
      )))
    

(defmacro ppme (form)
  `(let ((*compilep* t) (*compiling* t)
	 (*print-gensym* nil) (*print-case* :downcase)
	 (*print-length* nil) (*print-level* nil)
	 )
     (pprint (macroexpand-1 ',form))
     ))

(defun non-array-error () (error "You are calling an array function on a non-array pvar"))

(defmacro case-pvar-array ((pvar) general-pvar-form array-pvar-form)
  (declare (ignore general-pvar-form))
  (assert (symbolp pvar))
  `(if (void-pvar-p ,pvar)
       (if (any-active-processors?)
	   (non-array-error)
	   ,pvar
	   )
       ,array-pvar-form
       ))

