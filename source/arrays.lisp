;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Base: 10 -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defconstant *max-index-pvar-length* #.(1+ (truncate (log *array-total-size-limit 2))))
(defconstant *max-single-index-pvar-length* #.(1+ (truncate (log *array-dimension-limit 2))))
(defconstant *max-array-rank-pvar-length* #.(1+ (truncate (log *array-rank-limit 2))))

;;; Put the names of the ALIASED versions of the array accessor
;;; functions on the property lists of the array accessor function
;;; names.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc
    #'(lambda (x) (setf (get x 'alias!!-function) (intern (concatenate 'string "ALIASED-" (symbol-name x)) '*SIM-i)))
    '(aref!! svref!! bit!! sbit!!)
    ))



;; array-pvar-p is defined in pvars.lisp


(defun vector-pvar-p (pvar)
  (and (array-pvar-p pvar) (eql 1 (array-pvar-rank pvar)))
  )


(*defun *map (function-of-pvar-arguments &rest pvars)

  (simple-pvar-argument!! &rest pvars)

  (safety-check
    (new-multiple-pvar-check pvars '*map)
    (assert (every #'array-pvar-p pvars) () "Some pvar argument to *map is not an array pvar")
    )
  (apply 'map nil function-of-pvar-arguments (mapcar 'pvar-array-displaced-array pvars))
  )


(defun amap!! (function-of-pvar-arguments &rest pvars)

  (simple-pvar-argument!! &rest pvars)

  (safety-check
    (new-multiple-pvar-check pvars 'amap!!)
    (assert (every #'array-pvar-p pvars) () "Some pvar argument to amap!! is not an array pvar")
    )

  (when (null pvars)
    (error "Implementation limitation.  You must provide at least one array argument to amap!!")
    )

  (let ((rank (*array-rank (first pvars)))
	(dimensions (*array-dimensions (first pvars)))
	)

    (safety-check
      (dolist (pvar (cdr pvars))
	(when (not (equal rank (*array-rank pvar)))
	  (error "All the arrays given to amap!! do not have the same rank.")
	  ))
      (when (not (eql 1 rank))
	(dolist (pvar (cdr pvars))
	  (when (not (equal dimensions (*array-dimensions pvar)))
	    (error "All the arrays given to amap!! do not have the same dimensions")
	    ))))

    (let* ((element-pvar-results-list
	     (apply 'map 'list function-of-pvar-arguments (mapcar 'pvar-array-displaced-array pvars))
	     )
	   (result-array-dimensions
	     (if (eql 1 rank)
		 (length element-pvar-results-list)
		 dimensions
		 ))
	   (result-array-element-type
	     (if (not (*or t!!))
		 '(pvar boolean)
		 `(pvar ,(type-of (pref (car element-pvar-results-list) (*min (self-address!!)))))
		 )))

      (cond
	((equal result-array-element-type '(pvar float))
	 (setq result-array-element-type '(pvar single-float))
	 )
	((equal result-array-element-type '(pvar complex))
	 (setq result-array-element-type '(pvar (complex single-float)))
	 ))

      (let ((new-array (make-array!! result-array-dimensions :element-type result-array-element-type)))

	(let ((displaced-array (pvar-array-displaced-array new-array)))
	  (do ((j 0 (1+ j)) (pvar-list element-pvar-results-list (cdr pvar-list)))
	      ((null pvar-list))
	    (*set (aref displaced-array j) (car pvar-list))
	    ))

	new-array
	
	))))


(defun make-array!!

	(dimensions
	  &key
	  (element-type '(pvar t) element-type-provided?)
	  (initial-element nil)
	  )

  (when (not element-type-provided?)
    (error "Currently, the :element-type keyword to make-array!! must be provided")
    )

  ;; Check the dimensions.  For now we assume the dimensions
  ;; must be scalars.  Obviously they eventually could be pvars
  ;; which would give you different sized (but same ranked)
  ;; arrays in each processor.  Different ranked pvars in
  ;; each processor would require dimensions to be a varying-length
  ;; vector pvar!

  (safety-check
    (assert dimensions () "Implementation deficiency: cannot create a *Lisp array with 0 dimensions.  Sorry.")
    (assert (or (integerp dimensions) (listp dimensions)))
    )

  (when (numberp dimensions) (setq dimensions (list dimensions)))

  (safety-check
    (assert (every #'(lambda (x) (and (integerp x) (not (minusp x)))) dimensions) ()
	    "One of the dimensions provided is not a non-negative integer"
	    ))
    
  ;; Check the element type.

  (let ((canonical-type (canonical-pvar-type element-type)))
    (when (null canonical-type)
      (setq canonical-type (list 'pvar element-type))
      (setq canonical-type (canonical-pvar-type canonical-type))
      (when (null canonical-type)
	(error "The element type ~S is not recognizable as nor coercible into a valid *Lisp type specifier" element-type)
	))

    (when (eq '* (length-pvar-type canonical-type))
      (error "Pvar arrays whose elements are of indeterminate size are not currently allowed.")
      )

    ;; Check the initial element.  If it is a scalar promote it to a pvar.

    (when (and initial-element (not (pvarp initial-element)))
      (setq initial-element (!! initial-element))
      )

    ;; Compute the type specifier for an array of this type
    ;; and allocate one on the stack.

    (let ((array-type (make-canonical-pvar-type 'array :element-type (cadr canonical-type) :dimensions dimensions)))

      (let ((array-pvar (make-pvar-based-on-canonical-pvar-type :stack array-type)))

	(setf (pvar-constant? array-pvar) nil)
	(setf (pvar-lvalue? array-pvar) nil)
	(setf (pvar-name array-pvar) 'MAKE-ARRAY!!-RETURN)

	;; If there is an initial element assign it to each element
	;; of the array.

	(when initial-element
	  (*map #'(lambda (element) (*set element initial-element)) array-pvar)
	  )

	array-pvar

	))))


;;;;
;;;;          *****  AREF!!  *****
;;;;

(defvar *value-of-first-active-processor-when-no-processors-active* nil)
(eval-when (:load-toplevel :execute)
  (setq *value-of-first-active-processor-when-no-processors-active* (gensym "NO-ACTIVE-PROCESSORS-"))
  )

(defun value-of-first-active-processor (pvar &key (return-pvar? nil))
  "If there are no active processors
   *value-of-first-active-processor-when-no-processors-active*
   is explicitly returned.
  "
  (let ((pvar-array (pvar-array pvar)))
    (do-for-selected-processors-internal (j)
      (let ((value (aref pvar-array j)))
	(return-from value-of-first-active-processor
	  (if (pvar-p value)
	      (if return-pvar?
		  value
		  (value-of-first-active-processor value)
		  )
	      value
	      ))))
    *value-of-first-active-processor-when-no-processors-active*
    ))


(defun check-number-of-indices (pvar pvar-indices function)
  (let ((number-of-indices (length pvar-indices))
	(rank (array-pvar-rank pvar))
	)
    (assert (eql rank number-of-indices) ()
	    "~S: You provided ~D indices, but the pvar array ~S only has ~D dimensions"
	    function number-of-indices pvar rank
	    )))


(defun check-that-index-pvars-are-within-bounds (pvar index-pvars function)
  (dotimes (j (array-pvar-rank pvar))
    (let ((dimension-limit (array-pvar-dimension pvar j))
          (index-array (pvar-array (nth j index-pvars)))
          )
      (with-simple-vectors (index-array)
        (declare (type fixnum dimension-limit))
        (do-for-selected-processors-internal (i)
          (let ((value (aref index-array i)))
            (assert (valid-integer-range-exclusive value 0 dimension-limit) ()
                    "For function ~S, the value, ~S, of index-pvar ~D, in processor ~D, is not within the dimension limit ~D"
                    function value (nth j index-pvars) i dimension-limit
                    )))))))


(defmacro with-indices-iterated-over-active-processors
	  ((processor-index-symbol element-pvar-symbol) (pvar index-pvars) &body body)
  (assert (and (symbolp processor-index-symbol) (symbolp element-pvar-symbol)))
  (let ((index-pvar-arrays-symbol (gensym "INDEX-PVARS-ARRAYS-"))
	(pvar-symbol (gensym "PVAR-"))
	(number-of-dimensions-symbol (gensym "NUMBER-OF-DIMENSIONS-"))
	(single-index-list-symbol (gensym "SINGLE-INDEX-LIST-"))
	(array-holding-pvars-symbol (gensym "ARRAY-HOLDING-PVARS-"))
	(i-symbol (gensym "I-"))
	(index-pvars-symbol (gensym "INDEX-PVARS-"))
	)
    `(let* ((,index-pvars-symbol ,index-pvars)
	    (,index-pvar-arrays-symbol (mapcar #'pvar-array ,index-pvars-symbol))
	    (,pvar-symbol ,pvar)
	    (,number-of-dimensions-symbol (length ,index-pvars-symbol))
	    (,single-index-list-symbol (make-list ,number-of-dimensions-symbol))
	    (,array-holding-pvars-symbol (if (general-pvar-p ,pvar-symbol) nil (pvar-array ,pvar-symbol)))
	    )
       (do-for-selected-processors-internal (,processor-index-symbol)
	 (dotimes (,i-symbol ,number-of-dimensions-symbol)
	   (setf (nth ,i-symbol ,single-index-list-symbol) (aref (nth ,i-symbol ,index-pvar-arrays-symbol) ,processor-index-symbol))
	   )
	 (let ((,element-pvar-symbol
		(if ,array-holding-pvars-symbol
		    (apply #'aref ,array-holding-pvars-symbol ,single-index-list-symbol)
		    (apply #'aref (pvar-array (aref (pvar-array ,pvar-symbol) ,processor-index-symbol)) ,single-index-list-symbol)
		    )))
	   ,@body
	   )))))


(defun svref!!-internal (vector-pvar index-pvar return-pvar aliased?)
  
  ;; If aliased? is true then return-pvar is nil and the actual
  ;; element pvar is returned.  Otherwise the data is copied to
  ;; return-pvar and return-pvar is returned.
  
  (when (pvar-constant-value index-pvar)
    (return-from svref!!-internal
      (svref!!-internal-constant-index vector-pvar (pvar-constant-value index-pvar) return-pvar aliased?)
      ))
  
  (when aliased? (assert (null return-pvar) () "Internal error.  Return-pvar provided but aliased? is true"))
  
  ;; The complicated case.  We retrieve a possibly different
  ;; element from each processor.  We cannot return an
  ;; alias in this case.
  
  (if aliased?
      (error "Internal error.  Attempt to alias with non-constant index.")
    (let* ((index-array (pvar-array index-pvar))
           (return-array (pvar-array return-pvar))
           (vector-holding-pvars (pvar-array vector-pvar))
           (vector-length (length vector-holding-pvars))
           )
      (with-simple-vectors (index-array vector-holding-pvars)
        (do-for-selected-processors-internal (j)
          (let ((index (aref index-array j)))
            (assert (array-in-bounds-p vector-holding-pvars index) ()
                    "The value of the index pvar ~S, in processor ~D, ~D, is not within the array bounds ~S"
                    vector-pvar j index vector-length
                    )
            (let* ((element-pvar (aref vector-holding-pvars index)))
              (if (general-pvar-p element-pvar)
                  (setf (aref return-array j) (aref (pvar-location element-pvar) j))
                (*setf (pref return-pvar j) (pref element-pvar j))
                ))))
        return-pvar
        ))))

(defun svref!!-internal-constant-index (vector-pvar constant-index return-pvar aliased?)
  (when aliased?
    (assert (null return-pvar) ()
            "Internal error.  Return-pvar provided but aliased? is true"
            ))
  (let ((vector-holding-pvars (pvar-array vector-pvar)))
    (with-simple-vectors (vector-holding-pvars)
      (safety-check
       (assert (array-in-bounds-p vector-holding-pvars constant-index) ()
               "The value of the constant index pvar ~S, ~D, is not within the array bounds ~S"
               vector-pvar constant-index (length vector-holding-pvars)
               ))
      (if aliased?
          (svref vector-holding-pvars constant-index)
        (progn (let ((temp (aref vector-holding-pvars constant-index)))
                 (*set return-pvar temp))
          return-pvar)
        ))))



(defun svref!! (vector-pvar index-pvar)
  (safety-check
    (new-two-pvar-check vector-pvar index-pvar 'svref!!)
    (assert (vector-pvar-p vector-pvar))
    )
  (let ((return-pvar (allocate-temp-pvar-given-canonical-pvar-type (array-pvar-canonical-element-type vector-pvar))))
    (setf (pvar-lvalue? return-pvar) t)
    (setf (pvar-constant? return-pvar) nil)
    (if (pvar-constant-value index-pvar)
	(svref!!-internal-constant-index vector-pvar (pvar-constant-value index-pvar) return-pvar nil)
	(svref!!-internal vector-pvar index-pvar return-pvar nil)
	)
    (setf (pvar-lvalue? return-pvar) nil)
    return-pvar
    ))


(defun aliased-svref!! (vector-pvar index-pvar)

  ;; Used by *SETF and the ALIAS!! mechanism when the index
  ;; pvar is constant.

  (safety-check
    (new-pvar-check-lvalue-no-vp-check vector-pvar 'aliased-svref!!)
    (new-pvar-check index-pvar 'aliased-svref!!)
    (assert (vector-pvar-p vector-pvar))
    )

  (let (constant-index)
    (cond
      ((setq constant-index (pvar-constant-value index-pvar))
       (svref!!-internal-constant-index vector-pvar (pvar-constant-value index-pvar) nil t)
       )
      ((= (setq constant-index (*min index-pvar)) (*max index-pvar))
       (svref!!-internal-constant-index vector-pvar constant-index nil t)
       )
      (t (error "You cannot use alias!! to alias into a vector with non-constant indices"))
      )))


(*defun 
 indirect-setf-svref!! (vector-pvar value-pvar index-pvar)
 
 ;; Used by *SETF when the index pvar is not obviously
 ;; a constant. 
 
 (safety-check
  (progn
    (new-pvar-check-lvalue vector-pvar 'indirect-setf-svref!!)
    (new-two-pvar-check index-pvar value-pvar 'indirect-setf-svref!!)
    (assert (vector-pvar-p vector-pvar))
    ))
 
 (cond
  
  ;; If the index is really constant, but we couldn't tell
  ;; at compile time, revert back to the constant setf method.
  
  ((pvar-constant? index-pvar)
   (let ((single-index (value-of-first-active-processor index-pvar)))
     (if (eql single-index *value-of-first-active-processor-when-no-processors-active*)
         nil
       (*setf (aref!! vector-pvar (!! single-index)) value-pvar)
       )))
  
  ;; Otherwise, first check the index pvar for invalid indices
  
  (t
   
   (let* ((index-array (pvar-array index-pvar))
          (vector-holding-pvars (pvar-array vector-pvar))
          (vector-length (length vector-holding-pvars))
          )
     
     (do-for-selected-processors-internal (j)
       (let ((index (aref index-array j)))
         (assert (array-in-bounds-p vector-holding-pvars index) ()
                 "The value of the index pvar ~S, in processor ~D, ~D, is not within the array bounds ~S"
                 vector-pvar j index vector-length
                 )))
     
     (cond
      
      ;; The value pvar is a general pvar.
      ;; do this as efficiently as possible.
      
      ((general-pvar-p value-pvar)
       (let ((value-array (pvar-array value-pvar)))
         (do-for-selected-processors-internal (j)
           (let ((index (aref index-array j)))
             (let* ((element-pvar (aref vector-holding-pvars index))
                    (element-pvar-array (pvar-array element-pvar))
                    (value (aref value-array j))
                    )
               (if (not (pvar-p value))
                   (setf (aref element-pvar-array j) value)
                 (*setf (pref element-pvar j) (pref value j))
                 ))))))
      
      ;; The value pvar is an array or a structure pvar.
      ;; Move the elements using PREF.
      
      (t
       (do-for-selected-processors-internal (j)
         (let* ((index (aref index-array j))
                (element-pvar (aref vector-holding-pvars index))
                )
           (*setf (pref element-pvar j) (pref value-pvar j))
           )))
      
      ))))
 
 (values)
 
 )


(defun bit!! (bit-pvar &rest index-pvars) (apply 'aref!! bit-pvar index-pvars))
(defun aliased-bit!! (bit-pvar &rest index-pvars) (apply 'aliased-aref!! bit-pvar index-pvars))
(defun sbit!! (bit-pvar &rest index-pvars) (apply 'aref!! bit-pvar index-pvars))
(defun aliased-sbit!! (bit-pvar &rest index-pvars) (apply 'aliased-aref!! bit-pvar index-pvars))


(defun aref!!-internal (array-pvar pvar-indices return-pvar aliased?)
  
  (when aliased? (assert (null return-pvar) () "Internal error.  Return-pvar provided but aliased? is true"))
  
  (let ((array-holding-pvars (pvar-array array-pvar))
        (all-indices-constants? (every #'pvar-constant-value pvar-indices))
        )
    
    (if all-indices-constants?
        
        (cond
         ((eql 1 (length pvar-indices))
          (svref!!-internal-constant-index array-pvar (pvar-constant-value (car pvar-indices)) return-pvar aliased?)
          )
         (t
          (safety-check
           (do ((dimension-index 0 (1+ dimension-index))
                (index-list pvar-indices (cdr index-list))
                )
               ((null index-list))
             (let ((constant-index (pvar-constant-value (car index-list))))
               (when (>= constant-index (array-dimension array-holding-pvars dimension-index))
                 (error "The constant index ~S, accessing dimension ~D of the array pvar ~S, ~@
                         which has dimensions ~S, is not within those dimensions"
                   constant-index dimension-index array-pvar (array-dimensions array-holding-pvars)
                   )))))
          (if aliased?
              (apply #'aref array-holding-pvars (mapcar #'pvar-constant-value pvar-indices))
            (progn
              (*set return-pvar (apply #'aref array-holding-pvars (mapcar #'pvar-constant-value pvar-indices)))
              return-pvar
              ))))
      
      (if aliased?
          (error "Internal error.  Attempt to alias with non-constant index.")
        (let ((return-array (pvar-array return-pvar)))
          (check-that-index-pvars-are-within-bounds array-pvar pvar-indices 'AREF!!)
          (with-indices-iterated-over-active-processors
           (j element-pvar) (array-pvar pvar-indices)
           (let ((value (aref (pvar-array element-pvar) j)))
             (if (not (pvar-p value))
                 (setf (aref return-array j) value)
               (*setf (pref return-pvar j) (pref value j))
               )))
          return-pvar
          ))
      
      )))


(defun aref!! (pvar &rest pvar-indices)

  (simple-pvar-argument!! pvar &rest pvar-indices)

  (safety-check
    (new-pvar-check pvar 'aref!!)
    (validate-all-pvars pvar-indices 'aref!!)
    )

  (case-pvar-array

    (pvar)

    nil

    ;; Make sure the number of pvar-indices agrees with the rank of
    ;; pvar-array.
      
    (progn

      (safety-check
	(check-number-of-indices pvar pvar-indices 'aref!!)
	)

      ;; If there is only 1 dimension, use svref!!
      ;; Otherwise use the general aref!!-internal
	
      (let ((return-pvar (allocate-temp-pvar-given-canonical-pvar-type (array-pvar-canonical-element-type pvar)))
	    (length (length pvar-indices))
	    )

	(setf (pvar-lvalue? return-pvar) t)
	(setf (pvar-constant? return-pvar) nil)
	(cond
	  ((and (eql 1 length) (pvar-constant-value (car pvar-indices)))
	   (svref!!-internal-constant-index pvar (pvar-constant-value (car pvar-indices)) return-pvar nil)
	   )
	  ((eql 1 length)
	   (svref!!-internal pvar (car pvar-indices) return-pvar nil)
	   )
	  (t (aref!!-internal pvar pvar-indices return-pvar nil))
	  )
	(setf (pvar-lvalue? return-pvar) nil)

	return-pvar
	      
	))))


(defun aliased-aref!! (pvar &rest index-pvars)

  (simple-pvar-argument!! pvar &rest index-pvars)

  (safety-check
    (new-pvar-check-lvalue-no-vp-check pvar 'aliased-aref!!)
    (validate-all-pvars index-pvars 'aliased-aref!!)
    )

  (let ((every-pvar-constant? (every #'pvar-constant-value index-pvars)))

    (when (not every-pvar-constant?)
     (setq every-pvar-constant? (every #'(lambda (pvar) (= (*max pvar) (*min pvar))) index-pvars))
      (if every-pvar-constant?
	  (setq index-pvars (mapcar #'(lambda (pvar) (!! (*max pvar))) index-pvars))
	  (error "You cannot create an alias for an AREF!! form given non-constant indices.")
	  ))

    (case-pvar-array
      (pvar)
      nil
      (aref!!-internal pvar index-pvars nil t)
      )))


(defmacro setf-aref!! (pvar value-pvar &rest index-pvars)

  (simple-pvar-argument!! value-pvar &rest index-pvars)

  (if (eql 1 (length index-pvars))
      `(indirect-setf-svref!! ,pvar ,value-pvar ,(car index-pvars))
      `(indirect-setf-aref!! ,pvar ,value-pvar ,@index-pvars)
      ))

(*defun 
 indirect-setf-aref!! (pvar value-pvar &rest index-pvars)
 
 ;; Used by *SETF when the index pvar is not obviously
 ;; a constant. 
 
 (safety-check
  (new-two-pvar-check pvar value-pvar 'indirect-setf-aref!!)
  (validate-all-pvars index-pvars 'indirect-setf-aref!!)
  )
 
 (flet
     
     ((internal-indirect-setf-aref!! 
       ()
       (safety-check
        (check-that-index-pvars-are-within-bounds pvar index-pvars '*SETF-AREF!!)
        )
       (cond
        ((general-pvar-p value-pvar)
         (with-indices-iterated-over-active-processors
          (j element-pvar) (pvar index-pvars)
          (let ((value-array (pvar-array value-pvar))
                (element-pvar-array (pvar-array element-pvar))
                )
            (let ((value (aref value-array j)))
              (if (not (pvar-p value))
                  (setf (aref element-pvar-array j) value)
                (*setf (pref element-pvar j) (pref value j))
                )))))
        (t
         (with-indices-iterated-over-active-processors
          (j element-pvar) (pvar index-pvars)
          (*setf (pref element-pvar j) (pref value-pvar j))
          )))))
   
   (case-pvar-array 
    
    (pvar)
    
    nil
    ;      (internal-indirect-setf-aref!!)
    
    (cond
     
     ;; If the indices are really constant, but we couldn't tell
     ;; at compile time, revert back to the constant setf method.
     
     ((every #'pvar-constant? index-pvars)
      (let ((single-index-list (mapcar #'value-of-first-active-processor index-pvars)))
        (if (member *value-of-first-active-processor-when-no-processors-active* single-index-list)
            nil
          (*copy-pvar (apply #'aliased-aref!! pvar (mapcar #'!! single-index-list)) value-pvar)
          )))
     
     ;; Otherwise, first check the index pvars for invalid indices
     
     (t (internal-indirect-setf-aref!!))
     
     )))
 
 (values)
 
 )

;       (let* ((index-arrays (mapcar #'pvar-array index-pvars))
;	      (number-of-dimensions (length index-pvars))
;	      (single-index-set (make-list number-of-dimensions))
;	      (array-holding-pvars (pvar-array pvar))
;	      )
;	 (do-for-selected-processors-internal (j)
;	   (dotimes (i number-of-dimensions)
;	     (setf (nth i single-index-set) (aref (nth i index-arrays) j))
;	     )
;	   (assert (apply #'array-in-bounds-p array-holding-pvars single-index-set) ()
;			"The values of the index pvars ~S, in processor ~D, ~S, are not within the array bounds ~S"
;			index-pvars j single-index-set (array-pvar-dimensions array-holding-pvars)
;			))
;
;	 (cond
;
;	   ((general-pvar-p value-pvar)
;	    (let ((value-array (pvar-array value-pvar)))
;	      (do-for-selected-processors-internal (j)
;		(dotimes (i number-of-dimensions)
;		  (setf (nth i single-index-set) (aref (nth i index-arrays) j))
;		  )
;		(let* ((element-pvar (apply #'aref array-holding-pvars single-index-set))
;		       (element-pvar-array (pvar-array element-pvar))
;		       (value (aref value-array j))
;		       )
;		  (if (not (pvar-p value))
;		      (setf (aref element-pvar-array j) value)
;		      (*setf (pref element-pvar j) (pref value j))
;		      )))))
;	   
;	   (t
;	    (do-for-selected-processors-internal (j)
;		(dotimes (i number-of-dimensions)
;		  (setf (nth i single-index-set) (aref (nth i index-arrays) j))
;		  )
;		(let* ((element-pvar (apply #'aref array-holding-pvars single-index-set)))
;		  (*setf (pref element-pvar j) (pref value-pvar j))
;		  )))
;
;	   )))))




;;;; ************************************************************
;;;; ************************************************************



;;; This is cheating, because we *Lisp itself doesn't allow arrays of
;;; element type T, but the simulator doesn't really give a damn.


(defun vector!! (&rest pvars)
  (simple-pvar-argument!! &rest pvars)
  (validate-all-pvars pvars 'vector!!)
  (let ((return-vector-pvar (make-pvar-based-on-canonical-pvar-type :stack `(pvar (array t (,(length pvars)))))))
    (setf (pvar-name return-vector-pvar) 'VECTOR!!-RETURN)
    (setf (pvar-lvalue? return-vector-pvar) t)
    (dotimes (j (length pvars))
      (*setf (svref!! return-vector-pvar (!! j)) (nth j pvars))
      )
    (setf (pvar-lvalue? return-vector-pvar) nil)
    return-vector-pvar
    ))

(*proclaim '(*defun setf-row-major-aref!!))

(defun array!! (dimensions &rest pvars)
  (unless (= (reduce #'* dimensions) (length pvars))
    (error "Dimensions ~S must have the save size as the number of pvars ~D provided to array!!."
	   dimensions (length pvars)
	   ))
  (let ((result (make-pvar-based-on-canonical-pvar-type :stack `(pvar (array t ,dimensions)))))
    (setf (pvar-name result) 'ARRAY!!-RETURN)
    (setf (pvar-lvalue? result) t)
    (dotimes (j (length pvars))
      (*setf (row-major-aref!! result (!! j)) (nth j pvars))
      )
    (setf (pvar-lvalue? result) nil)
    (setf (pvar-array-dimensions result) dimensions)
    result
    ))


(defun typed-vector!! (element-type &rest pvars)
  (simple-pvar-argument!! &rest pvars)
  (safety-check (validate-all-pvars pvars 'typed-vector!!))
  (let ((element-type (valid-pvar-type-p element-type)))
    (let ((return-vector-pvar (make-array!! (length pvars) :element-type element-type)))
      (setf (pvar-name return-vector-pvar) 'TYPED-VECTOR!!-RETURN)
      (setf (pvar-lvalue? return-vector-pvar) t)
      (dotimes (j (length pvars))
	(*setf (aref!! return-vector-pvar (!! j)) (nth j pvars))
	)
      (setf (pvar-lvalue? return-vector-pvar) nil)
      return-vector-pvar
      )))

(defun every-active-processor-of-a-general-pvar-contains-an-array? (general-pvar)
  ;; Note.  If there are no active processors returns T.
  (let ((general-pvar-array (pvar-array general-pvar)))
    (with-simple-vectors (general-pvar-array)
      (do-for-selected-processors-internal (j)
        (when (not (array-pvar-p (aref general-pvar-array j)))
          (return-from every-active-processor-of-a-general-pvar-contains-an-array? nil)
          ))))
  t
  )


(*defun *array-element-type (pvar-array)
  "Returns to the front end the element type of pvar-array.
   It is an error if pvar-array were to be a general-pvar array
   containing different array types.
  "
  (simple-pvar-argument!! pvar-array)
  (safety-check (new-pvar-check pvar-array '*array-element-type))
  (case-pvar-array
    (pvar-array)
    nil
;    (if (no-processors-active)
;	nil
;	(array-pvar-canonical-element-type (get-single-active-array-in-general-pvar pvar-array '*array-element-type))
;	)
    (array-pvar-canonical-element-type pvar-array)
    ))






(defun array-rank!! (pvar-array)
  "Returns the number of dimensions of pvar-array as a pvar."

  (simple-pvar-argument!! pvar-array)

  (case-pvar-array
    (pvar-array)
    nil
    (!! (*array-rank pvar-array))
    ))


(*defun *array-dimension (pvar-array axis-number-scalar)
  "Returns to the front end the length of the
   axis-number-scalar dimension of pvar-array.
   It is an error if pvar-array were to be a
   general-pvar array containing different size arrays.
  "
  (simple-pvar-argument!! pvar-array)
  (safety-check
    (new-pvar-check pvar-array '*array-dimension)
    (assert (and (integerp axis-number-scalar) (not (minusp axis-number-scalar))))
    )
  (case-pvar-array
    (pvar-array)
    nil
    (progn
      (assert (< axis-number-scalar (array-pvar-rank pvar-array)) ()
	      "There are only ~D dimensions in the pvar array ~S but you asked for dimension ~D"
	      (array-pvar-rank pvar-array) pvar-array axis-number-scalar
	      )
      (array-pvar-dimension pvar-array axis-number-scalar)
      )))


(defun array-dimension!! (pvar-array dimension-pvar)

  "Returns the dimension size of a particular dimension in each processor."

  (simple-pvar-argument!! (pvar-array dimension-pvar))

  (safety-check
    (new-pvar-check dimension-pvar 'array-dimension!!)
    (assert (*and (integerp!! dimension-pvar)) ()
	    "The dimension-pvar argument is not everywhere an integer"
	    )
    (assert (*and (and!! (not!! (minusp!! dimension-pvar)) (<!! dimension-pvar (array-rank!! pvar-array)))) ()
	    "The dimension-pvar argument to array-dimension!! is not everywhere less than the rank of the array"
	    )
    )

  (*let (result)

    (let ((result-array (pvar-array result))
	  (dimension-pvar-array (pvar-array dimension-pvar))
	  )

      (case-pvar-array
	
	(pvar-array)
	
	;; A general pvar containing arrays
	
	nil
;	(if (no-processors-active)
;	    (!! 0)
;	    (progn
;	      (when (not (every-active-processor-of-a-general-pvar-contains-an-array? pvar-array))
;		(error "Not every active processor of pvar ~S contains an array" pvar-array)
;		)
;	      (let ((general-pvar-array (pvar-array pvar-array)))
;		(do-for-selected-processors-internal (j)
;		  (setf (aref result-array j) (array-pvar-dimension (aref general-pvar-array j) (aref dimension-pvar-array j)))
;		  ))))
;	
	;; An array pvar
	
	(let ((array-dimensions (array-pvar-dimensions pvar-array)))
	  (do-for-selected-processors-internal (j)
	    (setf (aref result-array j) (nth (aref dimension-pvar-array j) array-dimensions))
	    ))
	
	))

    result

    ))


(*defun *array-dimensions (pvar-array)
  "Returns to the front end as a list the dimension sizes of pvar-array.
   It is an error if pvar-array were to be a general-pvar array
   containing different size arrays.
  "
  (simple-pvar-argument!! pvar-array)
  (safety-check (new-pvar-check pvar-array '*array-dimensions))
  (case-pvar-array
    (pvar-array)
    nil
    (array-pvar-dimensions pvar-array)
    ))


(defun array-dimensions!! (pvar-array)
  "Returns a vector pvar of length the rank of the array,
   the nth element containing the nth dimension size.
  "
  (simple-pvar-argument!! pvar-array)

  (flet
    ((foo ()
       (apply 'typed-vector!!
	 `(pvar (unsigned-byte ,*max-index-pvar-length*))
	 (mapcar #'!! (*array-dimensions pvar-array))
	 )))
    (safety-check (new-pvar-check pvar-array 'array-dimensions!!))
    (case-pvar-array
      (pvar-array)
      nil
      (foo)
      )))


(*defun *array-total-size (pvar-array)
  "Returns to the front end the total number of elements in pvar-array.
   It is an error if pvar-array were to be a general-pvar array
   containing different size arrays.
  "
  (simple-pvar-argument!! pvar-array)
  (safety-check (new-pvar-check pvar-array '*array-total-size))
  (case-pvar-array
    (pvar-array)
    nil
    (apply #'* (*array-dimensions pvar-array))
    ))


(defun array-total-size!! (pvar-array)

  (simple-pvar-argument!! pvar-array)

  (case-pvar-array
    (pvar-array)
    nil
    (!! (*array-total-size pvar-array))
    ))


(defun apply-array-function-over-multiple-indices (pvar-array pvar-subscripts scalar-array-function)

  (*let (result)
    
    (let* ((number-of-dimensions (length pvar-subscripts))
	   (single-index-set (make-list number-of-dimensions))
	   (result-array (pvar-array result))
	   (index-arrays (mapcar #'pvar-array pvar-subscripts))
	   )
      
      (case-pvar-array
	
	(pvar-array)
	
	;; General pvar containing array pvars.
	
	nil
;	(when (some-processor-active)
;	  (multiple-value-bind (any-arrays? all-same-rank? all-same-shape?)
;	      (check-general-pvar-arrays-for-existence-and-same-rank-and-shape pvar-array)
;	    all-same-shape?
;	    (when (or (not any-arrays?) (not all-same-rank?))
;	      (error "The arrays in general pvar ~S are not all the same rank" pvar-array)
;	      )
;	    (let ((general-pvar-array (pvar-array pvar-array)))
;	      (do-for-selected-processors-internal (j)
;		(dotimes (i number-of-dimensions)
;		  (setf (nth i single-index-set) (aref (nth i index-arrays) j))
;		  )
;		(let* ((array-pvar-in-general-pvar (aref general-pvar-array j))
;		       (array-holding-pvars (pvar-array array-pvar-in-general-pvar))
;		       )
;		  (setf (aref result-array j) (apply scalar-array-function array-holding-pvars single-index-set))
;		  )))))
;	
	;; An array pvar.
	
	(progn
	  
	  (assert (eql number-of-dimensions (*array-rank pvar-array)) ()
		  "You provided ~D subscripts, but the pvar array ~S has only ~D dimensions"
		  number-of-dimensions pvar-array (*array-rank pvar-array)
		  )
	  
	  (let ((array-holding-pvars (pvar-array pvar-array)))
	    (do-for-selected-processors-internal (j)
	      (dotimes (i number-of-dimensions)
		(setf (nth i single-index-set) (aref (nth i index-arrays) j))
		)
	      (setf (aref result-array j) (apply scalar-array-function array-holding-pvars single-index-set))
	      )))
	
	))
    
    result
    
    ))


(defun array-in-bounds-p!! (pvar-array &rest pvar-subscripts)
  "Returns a boolean pvar which is T in every processor
   in which pvar-subscripts represents a valid reference
   to pvar-array, and NIL otherwise.
  "
  (simple-pvar-argument!! pvar-array &rest pvar-subscripts)

  (safety-check
    (new-pvar-check pvar-array 'array-in-bounds-p!!)
    (validate-all-pvars pvar-subscripts 'array-in-bounds-p!!)
    )
  (apply-array-function-over-multiple-indices pvar-array pvar-subscripts #'array-in-bounds-p)
  )


(defun array-row-major-index!! (pvar-array &rest pvar-subscripts)
  "Returns a pvar identifying the row-major index represented
   by pvar-subscripts in each processor.
   See array-row-major-index in CLtL for details.
  "
  (simple-pvar-argument!! pvar-array &rest pvar-subscripts)

  (safety-check
    (new-pvar-check pvar-array 'array-row-major-index!!)
    (validate-all-pvars pvar-subscripts 'array-row-major-index!!)
    )
  (apply-array-function-over-multiple-indices pvar-array pvar-subscripts #'array-row-major-index)
  )


(defun check-for-equal-array-sizes (function b1 b2 b3)
  (assert (equal (array-pvar-dimensions b1) (array-pvar-dimensions b2)) ()
	  "For function ~S, the arrays ~S and ~S do not have the same dimensions"
	  function b1 b2
	  )
  (when (pvarp b3)
    (assert (equal (array-pvar-dimensions b1) (array-pvar-dimensions b3)) ()
	    "For function ~S, the destination array ~S is not the same shape and size as the source arrays"
	    function b3
	    )))

(defun generic-boolean-array-function (function-name lisp-logical-operator b1 b2 b3)
  (safety-check
    (assert (and (array-pvar-p b1) (array-pvar-p b2) (or (member b3 '(t nil)) (array-pvar-p b3))) ()
	    "The arguments provided to ~S are not all array pvars"
	    function-name
	    ))
  (let* ((b1-array-holding-pvars (pvar-array b1))
	 (b1-displaced-array-holding-pvars (pvar-array-displaced-array b1))
	 (dimensions (array-dimensions b1-array-holding-pvars))
	 (total-size (array-total-size b1-array-holding-pvars))
	 (b2-displaced-array-holding-pvars (pvar-array-displaced-array b2))
	 (b3
	   (cond
	     ((eq t b3) b1)
	     ((eq nil b3) (make-array!! dimensions :element-type '(pvar (unsigned-byte 1))))
	     ((array-pvar-p b3) b3)
	     (t (error "You shouldn't get here!"))
	     ))
	 (b3-displaced-array-holding-pvars (pvar-array-displaced-array b3))
	 )
    (dotimes (i total-size)
      (safety-check
	(assert (and (general-pvar-p (aref b1-displaced-array-holding-pvars i))
		     (general-pvar-p (aref b2-displaced-array-holding-pvars i))
		     (general-pvar-p (aref b3-displaced-array-holding-pvars i))
		     )
		()
		"The array pvars provided to function ~S are not all arrays of bits"
		function-name
		))
      (let ((b1-element-array (pvar-array (aref b1-displaced-array-holding-pvars i)))
	    (b2-element-array (pvar-array (aref b2-displaced-array-holding-pvars i)))
	    (b3-element-array (pvar-array (aref b3-displaced-array-holding-pvars i)))
	    )
	(do-for-selected-processors-internal (j)
	  (let ((b1-bit (aref b1-element-array j))
		(b2-bit (aref b2-element-array j))
		)
	    (safety-check
	      (assert (and (member b1-bit '(0 1)) (member b2-bit '(0 1))) ()
		      "The contents of the arrays provided to ~S are not all ones and zeroes"
		      function-name
		      ))
	    (let ((numerical-result (funcall lisp-logical-operator b1-bit b2-bit)))
	      (setf (aref b3-element-array j) (if (not (minusp numerical-result)) numerical-result (+ numerical-result 2)))
	      )))))
    b3
    ))


(defmacro make-logical-boolean-array-functions (name-lisp-logical-operator-pairs)
  `(progn
     ,@(mapcar
	 #'(lambda (name-lisp-logical-operator-pair)
	     (let ((name (first name-lisp-logical-operator-pair))
		   (lisp-logical-operator (second name-lisp-logical-operator-pair))
		   )
	       `(defun ,name (bool-array-1 bool-array-2 &optional bool-dest-array)
		  (simple-pvar-argument!! bool-array-1 bool-array-2 &opt bool-dest-array)
		  (safety-check
		    (new-two-pvar-check bool-array-1 bool-array-2 ',name)
		    (when (pvarp bool-dest-array) (new-pvar-check bool-dest-array ',name))
		    )
		  (check-for-equal-array-sizes ',name bool-array-1 bool-array-2 bool-dest-array)
		  (generic-boolean-array-function ',name ',lisp-logical-operator bool-array-1 bool-array-2 bool-dest-array)
		  )))
	 name-lisp-logical-operator-pairs
	 )))

(make-logical-boolean-array-functions
  ((bit-and!! logand)
   (bit-ior!! logior)
   (bit-xor!! logxor)
   (bit-eqv!! logeqv)
   (bit-nand!! lognand)
   (bit-nor!! lognor)
   (bit-andc1!! logandc1)
   (bit-andc2!! logandc2)
   (bit-orc1!! logorc1)
   (bit-orc2!! logorc2)
   ))


(defun bit-not!! (boolean-pvar-array &optional boolean-result-pvar-array)
  (simple-pvar-argument!! boolean-pvar-array &opt boolean-result-pvar-array)
  (safety-check
    (new-pvar-check boolean-pvar-array 'bit-not!!)
    (assert (array-pvar-p boolean-pvar-array))
    (if (pvarp boolean-result-pvar-array)
	(progn
	  (new-pvar-check boolean-result-pvar-array 'bit-not!!)
	  (assert (array-pvar-p boolean-result-pvar-array))
	  )
	(assert (member boolean-result-pvar-array '(t nil)))
	))

  (let* ((b1-array-holding-pvars (pvar-array boolean-pvar-array))
	 (dimensions (array-dimensions b1-array-holding-pvars))
	 (total-size (array-total-size b1-array-holding-pvars))
	 (b1-displaced-array-holding-pvars (pvar-array-displaced-array boolean-pvar-array))
	 (boolean-result-pvar-array
	   (cond
	     ((eq t boolean-result-pvar-array) boolean-pvar-array)
	     ((eq nil boolean-result-pvar-array) (make-array!! dimensions :element-type '(pvar (unsigned-byte 1))))
	     ((array-pvar-p boolean-result-pvar-array) boolean-result-pvar-array)
	     (t (error "You shouldn't get here!"))
	     ))
	 (b2-displaced-array-holding-pvars (pvar-array-displaced-array boolean-result-pvar-array))
	 )

    (safety-check
      (assert (equal (array-pvar-dimensions boolean-pvar-array) (array-pvar-dimensions boolean-result-pvar-array)) ()
	      "For function ~S, the arrays ~S and ~S do not have the same dimensions"
	      'bit-not!! boolean-pvar-array boolean-result-pvar-array
	      ))

    (dotimes (i total-size)
      (safety-check
	(assert (and (general-pvar-p (aref b1-displaced-array-holding-pvars i))
		     (general-pvar-p (aref b2-displaced-array-holding-pvars i))
		     )
		()
		"The array pvars provided to function ~S are not all arrays of bits"
		'bit-not!!
		))
      (let ((b1-element-array (pvar-array (aref b1-displaced-array-holding-pvars i)))
	    (b2-element-array (pvar-array (aref b2-displaced-array-holding-pvars i)))
	    )
	(do-for-selected-processors-internal (j)
	  (let ((b1-bit (aref b1-element-array j)))
	    (safety-check
	      (assert (member b1-bit '(0 1)) ()
		      "The contents of the source array provided to ~S are not all ones and zeroes"
		      'bit-not!!
		      ))
	    (setf (aref b2-element-array j) (if (zerop b1-bit) 1 0))
	    ))))
    boolean-result-pvar-array
    ))


(*defun *arrayp (pvar)
  (simple-pvar-argument!! pvar)
  (safety-check (new-pvar-check pvar '*arrayp))
  (case-pvar-array
    (pvar)
    nil
    (array-pvar-p pvar)
    ))


(defun *vectorp (pvar)
  (simple-pvar-argument!! pvar)
  (safety-check (new-pvar-check pvar '*vectorp))
  (case-pvar-array
    (pvar)
    nil
;    (block exit
;      (let ((general-pvar-array (pvar-array pvar)))
;	(do-for-selected-processors-internal (j)
;	  (let ((value (aref general-pvar-array j)))
;	    (when (not (and (array-pvar-p value) (vectorp (pvar-array value))))
;	      (return-from exit nil)
;	      ))))
;      t
;      )
    (vector-pvar-p pvar)
    ))


(defun arrayp!! (pvar)
  (simple-pvar-argument!! pvar)
  (safety-check (new-pvar-check pvar 'arrayp!!))
  (cond
    ((array-pvar-p pvar) t!!)
;    ((general-pvar-without-arrays-p pvar) nil!!)
;    ((general-pvar-p pvar)
;     (*let (result)
;       (declare (type (pvar boolean) result))
;       (with-scalar-body-mapped-into-result-pvar (result) (pvar) #'(lambda (x) (and (pvar-p x) (array-pvar-p x))))
;       result
;       ))
    (t nil!!)
    ))


(defun vectorp!! (pvar)
  (simple-pvar-argument!! pvar)
  (safety-check (new-pvar-check pvar 'vectorp!!))
  (cond
    ((vector-pvar-p pvar) t!!)
;    ((general-pvar-without-arrays-p pvar) nil!!)
;    ((general-pvar-p pvar)
;     (*let (result)
;       (declare (type (pvar boolean) result))
;       (with-scalar-body-mapped-into-result-pvar (result) (pvar) #'(lambda (x) (and (pvar-p x) (vector-pvar-p x))))
;       result
;       ))
    (t nil!!)
    ))


(defun copy-array!! (array-pvar)
  (simple-pvar-argument!! array-pvar)
  (case-pvar-array
    (array-pvar)
    nil
;    (progn
;      (assert (every-active-processor-of-a-general-pvar-contains-an-array? array-pvar))
;      (*let ((result array-pvar)) result)
;      )
    (let ((result (make-array!! (*array-dimensions array-pvar) :element-type (*array-element-type array-pvar))))
      (setf (pvar-lvalue? result) t)
      (*set result array-pvar)
      (setf (pvar-lvalue? result) nil)
      result
      )))


(defun array-pvar-p-check (pvar function-name)
  (new-pvar-check pvar function-name)
  (assert (array-pvar-p pvar) () "~S called with non-array-pvar argument ~S" function-name pvar)
  )

(defun *sideways-array (pvar)
  (safety-check
    (array-pvar-p-check pvar '*sideways-array)
    )
  (setf (pvar-sideways-p pvar) (not (pvar-sideways-p pvar)))
  nil
  )

(*defun *slicewise (pvar)
  (safety-check
    (array-pvar-p-check pvar '*slicewise)
    )
  (if (pvar-sideways-p pvar)
      (error "Array is already sideways")
      (setf (pvar-sideways-p pvar) t)
      ))

(*defun *processorwise (pvar)
  (safety-check
    (array-pvar-p-check pvar 'processorwise)
    )
  (if (not (pvar-sideways-p pvar))
      (error "Array is already processorwise")
      (setf (pvar-sideways-p pvar) nil)
      ))

(defun sideways-aref!! (pvar &rest pvar-indices)
  (safety-check
    (array-pvar-p-check pvar 'sideways-aref!!)
    (assert (pvar-sideways-p pvar) () "sideways-aref!! called with array pvar ~S that is is not sideways" pvar)
    )
  (apply 'aref!! pvar pvar-indices)
  )

(defun setf-sideways-aref!! (pvar value &rest indices)

  (simple-pvar-argument!! value &rest indices)

  (safety-check
    (array-pvar-p-check pvar 'setf-sideways-aref!!)
    )
  (assert (pvar-sideways-p pvar) () "sideways-aref!! called with array pvar ~S that is is not sideways" pvar)
  (*apply 'indirect-setf-aref!! pvar value indices)
  )

(defun general-all-aref-indirect-aset!!
       
       (array-pvar
	list-of-accessor-functions
	list-of-list-of-indices
	value-pvar
	)

  (simple-pvar-argument!! value-pvar)
  
  ;; For each active processor...

  (let ((first-processor t)
	(reverse-list-of-accessor-functions (reverse list-of-accessor-functions))
	(reverse-list-of-list-of-indices (reverse list-of-list-of-indices))
	)

    (do-for-selected-processors-internal (processor)

      ;; descend the indirection tree all the way to the bottom,
      ;; retrieving the pvar we will modify.

      (let ((pvar-containing-values-to-change array-pvar))

	(mapcar
	  #'(lambda (accessor-function indices-pvars)
	      accessor-function
	      (when first-processor
		(safety-check
		  (new-pvar-check array-pvar '*setf-aref!!)
		  (assert (array-pvar-p array-pvar) ()
			  "The array pvar argument to a call to (*SETF (AREF!! ... was not an array pvar"
			  )
		  (new-multiple-pvar-check indices-pvars '*setf-aref!!)
		  (check-number-of-indices pvar-containing-values-to-change indices-pvars '*setf-aref!!)
		  (check-that-index-pvars-are-within-bounds pvar-containing-values-to-change indices-pvars '*setf-aref!!)
		  ))
	      (setq pvar-containing-values-to-change
		    (apply 'aref
			   (pvar-array pvar-containing-values-to-change)
			   (mapcar #'(lambda (index-pvar) (aref (pvar-location index-pvar) processor)) indices-pvars)
			   )))
	  reverse-list-of-accessor-functions
	  reverse-list-of-list-of-indices
	  )

	(setq first-processor nil)

	;; Modify it in that processor.

;	(ppp pvar-containing-values-to-change :end 10)
;	(print (list processor (pref value-pvar processor)))

	(*setf (pref pvar-containing-values-to-change processor) (pref value-pvar processor))

	)))

  (values)

  )



(defun row-major-aref!! (array-pvar index-pvar)
  (simple-pvar-argument!! array-pvar index-pvar)
  (safety-check
    (new-two-pvar-check array-pvar index-pvar 'row-major-aref!!)
    (assert (array-pvar-p array-pvar))
    )
  (let ((return-pvar (allocate-temp-pvar-given-canonical-pvar-type (array-pvar-canonical-element-type array-pvar))))
    (setf (pvar-lvalue? return-pvar) t)
    (setf (pvar-constant? return-pvar) nil)
    (let* ((array-holding-pvars (pvar-array array-pvar))
	   (displaced-array
	    (coerce
	     (make-array
	       (array-total-size array-holding-pvars)
	       :element-type (array-element-type array-holding-pvars)
	       :displaced-to array-holding-pvars
	       )
	     'simple-array)))
      (setf (pvar-array array-pvar) displaced-array)
      (unwind-protect
	  (progn
	    (if (pvar-constant-value index-pvar)
		(svref!!-internal-constant-index array-pvar (pvar-constant-value index-pvar) return-pvar nil)
		(svref!!-internal array-pvar index-pvar return-pvar nil)
		)
	    (setf (pvar-lvalue? return-pvar) nil)
	    return-pvar
	    )
	(setf (pvar-array array-pvar) array-holding-pvars)
	))))


(defun row-major-sideways-aref!! (array-pvar index-pvar)
  (safety-check
    (array-pvar-p-check array-pvar 'row-major-sideways-aref!!)
    (assert (pvar-sideways-p array-pvar) ()
	    "row-major-sideways-aref!! called with array pvar ~S that is is not sideways"
	    array-pvar))
  (funcall 'row-major-aref!! array-pvar index-pvar)
  )


(*defun setf-row-major-aref!! (array-pvar value-pvar index-pvar)

  (simple-pvar-argument!! value-pvar index-pvar)

  (safety-check

    (array-pvar-p-check array-pvar 'setf-row-major-aref!!)
    (new-pvar-check value-pvar 'setf-row-major-aref!!)
    (new-pvar-check index-pvar 'setf-row-major-aref!!)

    (*when (or!! (not!! (integerp!! index-pvar))
		 (minusp!! index-pvar)
		 (>=!! index-pvar (!! (*array-total-size array-pvar)))
		 )
      (when (*or t!!)
	(error "Invalid index given to 'setf-row-major-aref!!.  First invalid index at processor ~D is ~S"
	       (*min (self-address!!)) (pref index-pvar (*min (self-address!!)))
	       )))

    )

  (let ((displaced-array (pvar-array-displaced-array array-pvar))
	(index-array (pvar-array index-pvar))
	)
 
    (cond

      ;; The value pvar is a general pvar.
      ;; do this as efficiently as possible.

      ((general-pvar-p value-pvar)
       (let ((value-array (pvar-array value-pvar)))
	 (do-for-selected-processors-internal (j)
	   (let ((index (aref index-array j)))
	     (let* ((element-pvar (aref displaced-array index))
		    (element-pvar-array (pvar-array element-pvar))
		    (value (aref value-array j))
		    )
	       (if (not (pvar-p value))
		   (setf (aref element-pvar-array j) value)
		   (*setf (pref element-pvar j) (pref value j))
		   ))))))

      ;; The value pvar is an array or a structure pvar.
      ;; Move the elements using PREF.

      (t
       (do-for-selected-processors-internal (j)
	 (let* ((index (aref index-array j))
		(element-pvar (aref displaced-array index))
		)
	   (*setf (pref element-pvar j) (pref value-pvar j))
	   )))

      )))


(*defun setf-row-major-sideways-aref!! (array-pvar value-pvar index-pvar)

  (simple-pvar-argument!! value-pvar index-pvar)

  (safety-check
    (array-pvar-p-check array-pvar 'setf-row-major-sideways-aref!!)
    (assert (pvar-sideways-p array-pvar) ()
	    "setf-row-major-sideways-aref!! called with array pvar ~S that is is not sideways"
	    array-pvar))
  (setf-row-major-aref!! array-pvar value-pvar index-pvar)
  )


;;; This stuff is here instead of in send.lisp because it
;;; needs things on property lists which are set up here.  Yuck.


(defun *send-with-queue (dest-pvar source-pvar address-pvar)

  (*with-vp-set
    (pvar-vp-set dest-pvar)
    (*setf (aref!! dest-pvar (!! 0)) (!! 0))
    )

  (let* ((index-pvar (alias!! (aref!! dest-pvar (!! 0))))
	 (index-pvar-array (pvar-location index-pvar))
	 (dest-array-of-pvars (pvar-location dest-pvar))
	 (address-array (pvar-location address-pvar))
	 (array-size (array-pvar-total-size dest-pvar))
	 )

  (do-for-selected-processors (j)
    (let* ((value (pref source-pvar j))
	   (address (aref address-array j))
	   (index (1+ (aref index-pvar-array address)))
	   (dest-element (aref dest-array-of-pvars index))
	   )
      (incf (aref index-pvar-array address))
      (unless (>= index array-size)
	(*setf (pref dest-element address) value)
	)))))


;(defun test-*pset-queue ()
;  (*warm-boot)
;  (*let ((source (self-address!!))
;	 (dest (make-array!! 10 :element-type '(field-pvar 32) :initial-element (!! 0)))
;	 )
;    (declare (type (field-pvar 32) source))
;    (declare (type (vector-pvar (unsigned-byte 32) 10) dest))
;    (*let ((address (floor!! (self-address!!) (!! 9))))
;      (declare (type (field-pvar 32) address))
;      (pppdbg address)
;      (pppdbg source)
;      (*pset :queue source dest address)
;      )
;    (dotimes (j 10)
;      (ppp (aref!! dest (!! j)))
;      )))


