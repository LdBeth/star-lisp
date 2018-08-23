;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-i; MUSER: YES-*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.

;;;> Bugs, comments and revisions due to porting can be sent to:
;;;> bug-starlisp@think.com.  Other than to Thinking Machines'
;;;> customers, no promise of support is intended or implied.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


#|

    PREF!! source-expression address-expression
           &key 
           (vp-set *current-vp-set*)
           (collision-mode nil)
         
Each expression must be macroexpanded to determine its real nature.

For source-expression there are two cases.
    -- it is a symbol
    -- it is an expression.

If source-expression is an expression the expression
must be evaluated in the context of those processors
which are being gotten from.  If the VP SET is not
the same as the current VP SET, the expression must
also be evaluated in the VP SET which is being 
gotten from.

For address-expression there are four cases.
    -- it is a symbol.
    -- it is a list beginning with GRID!!
    -- it is a list beginning with GRID-RELATIVE!!
    -- it is some other expression.

The vp-set argument can be any expression.  

If the VP SET evaluates to or is NIL
   then
   if the source-expression is a symbol,
      then
      the VP SET is determined by the VP SET of the pvar 
      to which the symbol evaluates
      else
      it is assumed that the VP SET is *current-vp-set*
      and it is an error if source-expression evaluates
      to a pvar in another VP SET.
   else
   if the VP SET does not evaluate to the value of *current-vp-set*
      then we are doing a get from another VP SET,
      else we are doing a get from the same VP SET.

The collision mode can be three things
    -- :no-collisions
    -- :collisions-allowed
    -- :many-collisions
    -- nil

If :no-collisions is specified a simple send and
send back is used.

If :collisions-allowed is specified a loop using
send and send back is used.  If this loop iterates
too many times, an error may be generated depending
on user settable parameters.

If :many-collisions is specified the many-collisions
algorithm is used.

If nil is specified or the argument is not provided the Paris instruction
cm:get is used, which uses the backwards routing capability
of the CM2.  This potentially takes significant amounts
of memory (36 bits per VP bank per petite cycle).

It is always an error if an attempt is made to get from a
non-existent processor.

|#



;;; This file contains all the code for communications between processors through the router


(defmacro pref-grid!! (pvar x-pvar y-pvar &key border-pvar (collision-mode :collisions-allowed))
  (if border-pvar
      (let ((address-symbol (gensym "ADDRESS-OBJECT-")))
	`(*let ((,address-symbol (cube-from-grid-address!! ,x-pvar ,y-pvar)))
	   (declare (type (field-pvar 32) ,address-symbol))
	   (declare (return-pvar-p t))
	   (if!! (>=!! ,address-symbol (!! *number-of-processors-limit*))
		 (pref!! ,pvar ,address-symbol :collision-mode ,collision-mode)
		 ,border-pvar
		 )))
      `(pref!! ,pvar (cube-from-grid-address!! ,x-pvar ,y-pvar) :collision-mode ,collision-mode)
      ))



(eval-when (:compile-toplevel :load-toplevel :execute)

(defun macroexpand-form-for-pref!! (form env)

  ;; Returns 2 values, the macroexpanded form and whether the form
  ;; can definitely be evaluated safely in the CSS, and does not
  ;; have to be evaluated in the source CSS and VP Set.

  (cond
    ((symbolp form) (values form t))
    ((not (listp form)) (values form t)) ;;; modified to allow scalar args WRS 9/16/90
;;;                           (error "Unrecognizable form, ~S, in pref!!" form))
    ((eq 'the (car form))
     (if (not (eql 3 (length form)))
	 (error "Bad THE form inside PREF!!.  ~S" form)
	 (multiple-value-bind (the-body-form evaluate-in-css?)
	     (macroexpand-form-for-pref!! (third form) env)
	   (values `(the ,(second form) ,the-body-form) evaluate-in-css?)
	   )))
    ((is-place form env) (values (recursive-alias!! form env) t))
    ((really-macroexpandable-p form env)
     (macroexpand-form-for-pref!! (macroexpand-1 form) env)
     )
    (t (values form nil))
    ))

)


(defmacro pref!!
	  
	  #-(OR :CCL :ALLEGRO KCL)
	  (
	   source-pvar
	   address-object-pvar-or-cube-address-pvar
	   &key
	   (vp-set nil vp-set-provided?)
	   (collision-mode nil)
	   &environment env
	   )
	  #+(OR :CCL :ALLEGRO KCL)
	  (
	   &environment env
	   source-pvar
	   address-object-pvar-or-cube-address-pvar
	   &key
	   (vp-set nil vp-set-provided?)
	   (collision-mode nil)
	   )
  
  ;; Macroexpand the source and address expressions.

  (multiple-value-bind (source-expression source-is-symbol)
      (macroexpand-form-for-pref!! source-pvar env)
    
    (multiple-value-bind (address-expression address-is-symbol)
	(macroexpand-form-for-pref!! address-object-pvar-or-cube-address-pvar env)
      
      ;; Make a closure over the source expression, if necessary.

      (let ((new-source-expression
	     (if source-is-symbol source-expression
		 `#'(lambda () (let ((%TEMP-NEW-PREF!!% ,source-expression))
				 (pvar-argument!! %TEMP-NEW-PREF!!% legal-pvar-value)
				 %TEMP-NEW-PREF!!%)))))
	  
	;; Two important different cases, whether the
	;; source is a symbol or whether it needs
	;; to be evaluated in the special right context.

	(if source-is-symbol

	    (cond
	    
	      (address-is-symbol
	       `(pref!!-address-expression
		  ,new-source-expression
		  ,address-expression
		  ,(if vp-set-provided? vp-set `(if (fast-pvarp ,new-source-expression)
						    (pvar-vp-set ,new-source-expression)
						    *current-vp-set*))
		  ,collision-mode
		  ))
	    
	      ;; The user specified GRID!!.  Try to convert the
	      ;; indices into an address object in the geometry
	      ;; of the source VP SET.  If this fails the user
	      ;; has given us illegal grid coordinates.

	      ((eq 'grid!! (car address-expression))
	       `(pref!!-address-expression
		  ,new-source-expression
		  (internal-cube-from-vp-grid-address!!
		    'pref!! t nil (pvar-vp-set ,new-source-expression) (list ,@(cdr address-expression))
		    )
		  ,(if vp-set-provided? vp-set `(pvar-vp-set ,new-source-expression))
		  ,collision-mode
		  ))
	    
	      ;; Same idea as above but for GRID-RELATIVE!!.

	      ((eq 'grid-relative!! (car address-expression))
	       `(pref!!-address-expression
		  ,new-source-expression
		  (internal-cube-from-vp-grid-address!!
		    'pref!! t t (pvar-vp-set ,new-source-expression) (list ,@(cdr address-expression))
		    )
		  ,(if vp-set-provided? vp-set `(pvar-vp-set ,new-source-expression))
		  ,collision-mode
		  ))
	    
	      ;; We have some random expression which can evaluate
	      ;; to either an address object or a cube address
	      ;; (or be invalid).

	      (t
	       `(pref!!-address-expression
		  ,new-source-expression
		  ,address-expression
		  ,(if vp-set-provided? vp-set `(if (fast-pvarp ,new-source-expression)
						    (pvar-vp-set ,new-source-expression)
						    *current-vp-set*))
		  ,collision-mode
		  ))

	      )
	    
	    ;; The source is an expression.

	    (cond
	      
	      ;; Evaluate the expression in the current VP SET carefully.

	      ((or (not vp-set-provided?) (eq '*current-vp-set* vp-set))
	       `(pref!!-closure
		  ,new-source-expression
		  ,address-expression
		  ,collision-mode
		  ))
	      
	      ;; Evaluate the expression in a user-specified VP SET carefully.

	      (t
	       `(vp-pref!!-closure
		  ,new-source-expression
		  ,address-expression
		  ,vp-set
		  ,collision-mode
		  ))
	      
	      ))))))


(defun pref!!-address-expression (source-pvar address-pvar vp-set collision-mode)

  (simple-pvar-argument!! source-pvar address-pvar)
  
  (safety-check
    (progn
      (vp-set-check vp-set 'pref!!)
      (assert (member collision-mode *allowed-collision-modes-for-pref!!*) () "Invalid collision mode")
      (new-pvar-check address-pvar 'pref!!)
      (assert (pvar-p source-pvar) () "Non pvar as source expression to PREF!!")
      ))

  ;; Make sure the source-pvar and the source-vp-set are consistent!
  
  (when (not (eq (pvar-vp-set source-pvar) vp-set))
    (error "PREF!!.  The source VP SET is ~A, but the :VP-SET argument value was ~A"
	   (pvar-vp-set source-pvar) vp-set
	   ))
    
    ;; We've now evaluated the address expression.
    ;; If it's a structure pvar it better be an
    ;; address object!
    
    ;; If it's an address object branch to the address object
    ;; routine, otherwise we assume it is a cube address and
    ;; branch to that routine.
    
    (cond
      ((eq (pvar-type address-pvar) :structure)
       (when (not (eq (pvar-structure-name address-pvar) 'address-object))
	 (error "Unknown address pvar given to PREF!!.  ~S.  You must provide either cube addresses or address objects"
		address-pvar
		))
       (pref!!-address-object source-pvar address-pvar vp-set collision-mode)
       )
      (t
       (safety-check (check-cube-address-pvar address-pvar (pvar-vp-set source-pvar) 'pref!!))
       (pref!!-cube-address source-pvar address-pvar collision-mode)
       ))

    )

(defun pref!!-address-object (source-pvar address-object vp-set collision-mode)
  (let ((coerced-address-object (smash-address-object-to-new-vp-set!! address-object vp-set)))
    (*let ((cube-address-pvar (address-object-cube-address!! (the (pvar address-object) coerced-address-object))))
      (pref!!-cube-address source-pvar cube-address-pvar collision-mode)
      )))


(defun pref!!-cube-address (source-pvar cube-address-pvar collision-mode)
  (pref!!-internal source-pvar cube-address-pvar collision-mode)
  )


(defun pref!!-closure (source-closure address-pvar collision-mode)

  (simple-pvar-argument!! address-pvar)
  
  ;; Save away the current selected set.
    
  (let!! (css)
    (declare (type (pvar boolean) css))
    (*all (declare (return-pvar-p nil)) (*set css nil!!))
    (*set css t!!)
      
    ;; Allocate a bit that is everywhere NIL!!.
      
    (all!!
      (let!! ((bit nil!!))
	(declare (type (pvar boolean) bit))
	  
	;; Select back the css and notify the processors
	;; we want to read from.
	  
	(*when css (declare (return-pvar-p nil)) (*nocompile (*pset :overwrite t!! bit address-pvar)))
	  
	;; Select the processors we are reading from
	;; and evaluate the source expression.
	  
	;; Select back the css and do a simple PREF!!.
	  
	(let (source)
	  (when!! bit
	    (setq source (funcall source-closure))
	    (all!!
	      (when!! css
		(pref!!-address-expression source address-pvar *current-vp-set* collision-mode)
		))))
	  
	))))


(defun vp-pref!!-closure (source-closure address-pvar read-from-vp-set collision-mode)
  
  (simple-pvar-argument!! address-pvar)
  
  (let ((reading-vp-set *current-vp-set*))
      
    (*with-vp-set read-from-vp-set
	
      ;; allocate a bit in the read-from vp set.
	
      (all!!
	(let!! ((bit nil!!))
	  (declare (type (pvar boolean) bit))
	    
	  ;; turn on those processors in the read-from
	  ;; vp set which we are reading from.
	    
	  (*with-vp-set reading-vp-set
	    (*nocompile (*pset :overwrite t!! bit address-pvar))
	    )
	    
	  ;; evaluate the source expression in those
	  ;; processors.  Go back to the reading VP SET
	  ;; and do a simple PREF!! on the evaluated
	  ;; result.
	    
	  (let (source)
	    (when!! bit
	      (setq source (funcall source-closure))
	      (when (not (eq (pvar-vp-set source) read-from-vp-set))
		(error "PREF!!.  You told me that the source expression was in VP SET ~A, but it is really in VP SET ~A"
		       read-from-vp-set (pvar-vp-set source)
		       ))
	      (*with-vp-set reading-vp-set
		(pref!!-address-expression source address-pvar read-from-vp-set collision-mode)
		)))
	    
	  )))))



(defun pref!!-internal (source-pvar send-address-pvar collision-mode)

  ;; This function allocates a result pvar of the type
  ;; of pvar that source-pvar is, or that the closure
  ;; returns.

  ;; It then permutes the source by the addresses and puts
  ;; the resulting reordering into the result.

  ;; Every time a processor is addressed, the
  ;; collision-count-pvar in that processor is incremented.

  ;; If any address is invalid, the invalid-address-pvar-or-closure
  ;; value is used instead of the source-pvar-or-closure value.

  ;; First compute the collision-count-pvar result.

      
  (safety-check
    (let ((current-vp-set *current-vp-set*))
      (when (eq collision-mode :no-collisions)
	(*let (css)
	      (*all (*set css nil!!))
	      (*set css t!!)
	      (*with-vp-set
		(pvar-vp-set source-pvar)
		(*all
		  (*let ((collision-count-pvar (!! 0)))
			(*with-vp-set
			  current-vp-set
			  (*all			    
			    (*when css
				   (*pset :add (!! 1) collision-count-pvar send-address-pvar)
				   )))
			(when (not (*and (<!! collision-count-pvar (!! 2))))
			  (error ":no-collisions was specified to PREF!! but collisions occurred.")
			  ))))))))

  (let ((result-pvar (allocate-similar-temp-pvar source-pvar))) ;; (allocate-temp-pvar-given-canonical-pvar-type (pvar-canonical-pvar-type source-pvar))))
    (setf (pvar-lvalue? result-pvar) t)
    (actual-pref!!-internal result-pvar source-pvar send-address-pvar)
    (setf (pvar-lvalue? result-pvar) nil)
    result-pvar
    )

  )


(defun actual-pref!!-internal (result-pvar source-pvar address-pvar)

  ;; Do a pref!!, handling every kind of pvar.

  (cond
    
    ;; A pvar containing only scalar quantities.
    ;; Just do the reordering.
    
    ((simple-general-pvar-p source-pvar)
     (with-selected-general-pvar-arrays
       (j) (result-array source-array address-array) (result-pvar source-pvar address-pvar)
       (setf (aref result-array j) (aref source-array (aref address-array j)))
       ))
    
    ;; An array pvar.  Recurse over each of its elements.
    
    ((array-pvar-p source-pvar)
     (let ((lisp-array-holding-source-pvars (pvar-array source-pvar))
	   (lisp-array-holding-result-pvars (pvar-array result-pvar))
	   )
       (with-many-array-elements-iterated
	 (result-element source-element)
	 (lisp-array-holding-result-pvars lisp-array-holding-source-pvars)
	 (actual-pref!!-internal result-element source-element address-pvar)
	 )))
    
    ;; A structure pvar.  Recursve over each of its slots.
    
    ((structure-pvar-p source-pvar)
     (let ((lisp-structure-holding-source-pvars (pvar-structure source-pvar))
	   (lisp-structure-holding-result-pvars (pvar-structure result-pvar))
	   )
       (with-structure-elements-iterated
	 (
	  (result-slot-pvar source-slot-pvar)
	  (lisp-structure-holding-result-pvars lisp-structure-holding-source-pvars)
	  (structure-pvar-type-front-end-slot-accessors
	    (pvar-canonical-pvar-type source-pvar)
	    ))
	 (actual-pref!!-internal result-slot-pvar source-slot-pvar address-pvar)
	 )))
    
    ;; A general pvar possibly containing arrays and structures.
    ;; Do it THE SLOW WAY, one processor at a time using (*SETF (PREF ...
    ;; If anyone ever really tries to do this they deserve all the time
    ;; it takes!
    
    (t
     (do-for-selected-processors-internal (j)
       (*setf (pref result-pvar j) (pref source-pvar (pref address-pvar j)))
       ))
    
    ))



(defun check-pvar-dimensionality (pvar dimensionality function-name)
  (when (not (eql dimensionality (length (vp-set-dimensions (pvar-vp-set pvar)))))
    (error
      "~S: You are trying to operate on a pvar in a ~D dimensional way, but the pvar ~S is defined in a vp set of ~D dimensions"
      function-name dimensionality pvar (length (vp-set-dimensions (pvar-vp-set pvar)))
      )))

(defun check-dimension-scalar (dimension-scalar function-name)
  (when (or (not (integerp dimension-scalar))
	    (< dimension-scalar 0)
	    (>= dimension-scalar *number-of-dimensions*)
	    )
    (error "~S:  You provided a dimension scalar argument ~S, but the number of dimensions is currently ~D"
	   function-name dimension-scalar *number-of-dimensions*
	   )))

(defun check-news!!-arguments (source-pvar indices border-pvar function-name)
  (safety-check
    (when (not (functionp source-pvar)) (new-pvar-check source-pvar function-name))
    (when (and border-pvar (not (functionp border-pvar))) (new-pvar-check border-pvar function-name))
    (when (not (functionp source-pvar)) (check-pvar-dimensionality source-pvar *number-of-dimensions* function-name))
    (when (/= (length indices) *number-of-dimensions*)
      (error "~S:  You provided ~D index arguments, but the number of dimensions currently is ~D"
	     function-name (length indices) *number-of-dimensions*
	     ))
    (dotimes (j (length indices))
      (let ((index (nth j indices)))
	(when (not (integerp index))
	  (error "~S:  Argument ~D, having value ~S, is not an integer.  The index arguments to ~S must be integers, not pvars"
		 function-name (1+ j) index function-name
		 ))))))


(defmacro news!! (pvar &rest indexes)
  (cond
    ((symbolp pvar)
     `(news!!-1 ,pvar ,@indexes))
    (t
     `(news!!-2 #'(lambda () ,pvar) ,@indexes)
     )))


(defun news!!-internal (pvar integers)
  (*let ((cube-address-to-fetch-from
	   (apply 'cube-from-grid-address!!
	     (mapcar
	       #'(lambda (integer dimension-index dimension-limit)
		   (mod!! (+!! (!! integer) (self-address-grid!! (!! dimension-index))) (!! dimension-limit))
		   )
	       integers
	       (iota *number-of-dimensions*)
	       (vp-set-dimensions *current-vp-set*)
	       ))))
    (pref!! pvar cube-address-to-fetch-from :collision-mode :no-collisions :vp-set *current-vp-set*)
    ))


(defun-wcefi news!!-1 (pvar &rest indexes)
  (simple-pvar-argument!! pvar)
  (check-news!!-arguments pvar indexes nil 'news!!)
  (news!!-internal pvar indexes)
  )


(defun-wcefi news!!-2 (pvar &rest indexes)

  (simple-pvar-argument!! pvar)
  (check-news!!-arguments pvar indexes nil 'news!!)

  ;; Figure out which processors are being read from.
  ;; Evaluate the lambda expression in that context.

  (*let (processors-not-reading-off-grid result)
    (declare (return-pvar-p t))
    (*all (*set processors-not-reading-off-grid nil!!))
    (*set processors-not-reading-off-grid t!!)
    (*all
      (declare (return-pvar-p nil))
      (*when (apply 'news!!-1 processors-not-reading-off-grid (mapcar '- indexes))
	(*set result (funcall pvar))
	))

    ;; Finally, do the news!! operation itself.

    (*set result (news!!-internal result indexes))

    result

    ))


(defmacro news-border!! (pvar border-pvar &rest indexes)
  (cond ((and (symbolp pvar) (symbolp border-pvar))
	 `(news-border!!-1 ,pvar ,border-pvar ,@indexes))
	(t
	 `(news-border!!-2 ,(if (symbolp pvar) pvar `#'(lambda () ,pvar))
			   ,(if (symbolp border-pvar) border-pvar `#'(lambda () ,border-pvar))
			   ,@indexes
			   ))))


(defun-wcefi news-border!!-1 (pvar border-pvar &rest indexes)
  (simple-pvar-argument!! pvar border-pvar)
  (check-news!!-arguments pvar indexes border-pvar 'news-border!!)
  (if!! (apply 'off-grid-border-relative-p!! (mapcar '!! indexes))
	border-pvar
	(news!!-internal pvar indexes)
	))


(defun-wcefi news-border!!-2 (pvar border-pvar &rest indexes)

  (simple-pvar-argument!! pvar border-pvar)
  (check-news!!-arguments pvar indexes border-pvar 'news-border!!)

  ;; Figure out which processors are actually reading off the grid.

  (*let (processors-not-reading-off-grid result)
    (declare (return-pvar-p t))
    (*all (*set processors-not-reading-off-grid nil!!))
    (*set processors-not-reading-off-grid
	  (not!! (apply 'off-grid-border-relative-p!! (mapcar '!! indexes)))
	  )

    ;; If pvar is a lambda expression, evaluate in the context
    ;; of those processors which are being gotten from.  Do the
    ;; news operation.  This is done including processors which
    ;; are reading off the edge, but that doesn't matter because
    ;; below we will overwrite the results in those processors.

    (cond
      ((functionp pvar)
       (*all
	 (declare (return-pvar-p nil))
	 (*when (news!!-internal processors-not-reading-off-grid (mapcar '- indexes))
	   (*set result (funcall pvar))
	   ))
       (*set result (news!!-internal result indexes))
       )
      (t
       (*set result (news!!-internal pvar indexes))
       ))

    ;; Now, for those processors which are really reading off
    ;; the edge, evaluate the border pvar instead.

    (*when (not!! processors-not-reading-off-grid)
      (*set result (if (functionp border-pvar) (funcall border-pvar) border-pvar))
      )

    result

    ))


(*defun *news (source destination &rest integers) 

   (simple-pvar-argument!! source)

;  (declare (return-pvar-p nil))

  ;; We need to figure out which processors are going to end
  ;; up with data.  Set up a bit which is T in active processors
  ;; and NIL otherwise, and use NEWS!! to grab at that bit using
  ;; the negation of the indices given to *NEWS.  This results
  ;; in every processor which we are to send to ending up with
  ;; a T, and other processors ending up with NIL.  

  (*compile-blindly
    (*let (processor-to-receive-data css)
      (declare (type (pvar boolean) processor-to-receive-data css))
      (declare (return-pvar-p nil))
      (*all
	(declare (return-pvar-p nil))
	(*set processor-to-receive-data nil!!)
	(*set css nil!!)
	)
      (*set css t!!)
      (*all (*set processor-to-receive-data
		  (the boolean-pvar (news!!-internal css (mapcar #'- integers)))))

      ;; Now send the source via the NEWS network in all
      ;; processors, but send it to a temporary instead
      ;; of to the actual destination.

      (*all
	(*nocompile
	  (*let ((temp (news!!-internal source (mapcar #'- integers))))
	    (declare (type (pvar *) temp))
	    (declare (return-pvar-p nil))

	    ;; Finally, in those processors we determined should receive data,
	    ;; copy the sent source into the destination.

	    (*compile-blindly
	      (*when processor-to-receive-data (*nocompile (*set destination temp)))
	      )

	    ))))))


(defmacro news-direction!! (pvar dimension-scalar distance-scalar)
  (let ((offsets-symbol (gensym "OFFSETS-"))
	(dimension-scalar-symbol (gensym "DIMENSION-SCALAR-"))
	)
    `(let ((,offsets-symbol (make-list *number-of-dimensions* :initial-element 0))
	   (,dimension-scalar-symbol ,dimension-scalar)
	   )
       (check-dimension-scalar ,dimension-scalar-symbol 'news-direction!!)
       (setf (nth ,dimension-scalar-symbol ,offsets-symbol) ,distance-scalar)
       ,(if (symbolp pvar)
	    `(apply 'news!!-1 ,pvar ,offsets-symbol)
	    `(apply 'news!!-2  #'(lambda () ,pvar) ,offsets-symbol)
	    ))))

(*defun *news-direction (source destination dimension-scalar distance-scalar)
  (let ((offsets (make-list *number-of-dimensions* :initial-element 0)))
    (check-dimension-scalar dimension-scalar '*news-direction)
    (setf (nth dimension-scalar offsets) distance-scalar)
    (*apply '*news source destination offsets)
    ))


(defun off-grid-border-relative-direction-p!! (dimension-scalar distance-scalar)
  (safety-check
    (check-dimension-scalar dimension-scalar 'off-grid-border-relative-direction-p!!)
    )
  (*let (result)
    (declare (type boolean-pvar result))
    (if (not (minusp distance-scalar))
	(*set result (>=!! (+!! (self-address-grid!! (!! (the fixnum dimension-scalar))) (!! (the fixnum distance-scalar)))
			   (!! (the fixnum (dimension-size dimension-scalar)))
			   ))
	(*set result (minusp!! (-!! (self-address-grid!! (!! (the fixnum dimension-scalar))) (!! (the fixnum distance-scalar)))))
	)
    result
    ))




