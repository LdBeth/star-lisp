;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Base: 10 -*-

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


;;;;  **************************************************************************

;;;;  THIS CODE IS SOURCE COMPATIBLE BETWEEN THE *LISP SIMULATOR AND THE
;;;;  *LISP INTERPRETER.  DO NOT CHANGE IT UNLESS YOU KNOW WHAT YOU ARE
;;;;  DOING AND CAN MAINTAIN THIS SOURCE COMPATIBILITY.

;;;; ***************************************************************************


(defun sequence-pvar-check (pvar function-name)
  (assert (vector-pvar-p pvar) ()
	  "The sequence-pvar argument ~S, to function ~S, is not a sequence pvar" pvar function-name
	  ))


(defun sequence-function-pvar-args-check (function-name sequence &rest pvars)
  (new-vp-pvar-check sequence function-name)
  (new-multiple-pvar-check pvars function-name)
  (sequence-pvar-check sequence function-name)
  )


(defmacro without-void-sequence-pvar (sequence-pvar function-name &body body)
  (assert (symbolp sequence-pvar))
  `(if (void-pvar-p ,sequence-pvar)
       (if (*or t!!) (sequence-pvar-check ,sequence-pvar ',function-name) ,sequence-pvar)
       (progn
	 (safety-check (sequence-pvar-check ,sequence-pvar ',function-name))
	 ,@body
	 )))


(defmacro without-void-sequence (sequence function-name &body body)
  (assert (symbolp sequence))
  `(progn
     (safety-check (new-vp-pvar-check ,sequence ',function-name))
     (without-void-sequence-pvar
       ,sequence ,function-name
       ,@body
       )))


(defun copy-seq!! (sequence)
  (simple-pvar-argument!! sequence)
  (without-void-sequence sequence copy-seq!!
    (copy-seq!!-internal sequence)
    ))


(defun fast-length!! (sequence) (!! (array-pvar-total-size sequence)))


(defun length!! (sequence)
  (simple-pvar-argument!! sequence)
  (without-void-sequence sequence length!!
    (fast-length!! sequence)
    ))


(*defun *nreverse (sequence)
  (without-void-sequence sequence *nreverse
    (*nreverse-internal sequence)
    ))


(defun reverse!! (sequence)
  (simple-pvar-argument!! sequence)
  (without-void-sequence sequence reverse!!
    (reverse!!-internal sequence)
    ))


(defun subseq!! (sequence start &optional (end (length!! sequence)))
  (simple-pvar-argument!! sequence start &opt end)
  (without-void-sequence sequence subseq!!
    (without-void-pvars (start end)
      (subseq!!-internal sequence start end)
      )))


(*proclaim '(ftype (function (t pvar &rest pvar) (pvar boolean)) some!! every!! notany!! notevery!!))


(defun find-shortest-length (sequence-pvar sequence-pvars)
  (let ((shortest-length (array-pvar-total-size sequence-pvar)))
    (dolist (pvar sequence-pvars)
      (setq shortest-length (min shortest-length (array-pvar-total-size pvar)))
      )
    shortest-length
    ))


(defun-wcefi some!! (predicate sequence &rest more-sequences)
  (simple-pvar-argument!! sequence &rest more-sequences)
  (safety-check
    (new-multiple-pvar-check more-sequences 'some!!)
    )
  (without-void-sequence sequence some!!
    (without-void-pvar-list more-sequences
      (let ((shortest-length (find-shortest-length sequence more-sequences)))
	(let!! ((result nil!!))
	  (declare (type boolean-pvar result))
	  (*map-vectors-nocheck
	    #'(lambda (&rest element-pvars)
		(*when (not!! result)
		  (*set result (or!! result (the (pvar boolean) (apply predicate element-pvars))))
		  ))
	    (!! 0)
	    (!! shortest-length)
	    nil nil
	    (cons sequence more-sequences)
	    )
	  result
	  )))))


(defun-wcefi every!! (predicate sequence &rest more-sequences)
  (simple-pvar-argument!! sequence &rest more-sequences)
  (safety-check
    (new-multiple-pvar-check more-sequences 'every!!)
    )
  (without-void-sequence sequence every!!
    (without-void-pvar-list more-sequences
      (let ((shortest-length (find-shortest-length sequence more-sequences)))
	(let!! ((result t!!))
	  (declare (type boolean-pvar result))
	  (*map-vectors-nocheck
	    #'(lambda (&rest element-pvars)
		(*when result
		  (declare (return-pvar-p nil))
		  (*set result (and!! result (the (pvar boolean) (apply predicate element-pvars))))
		  ))
	    (!! 0)
	    (!! shortest-length)
	    nil nil
	    (cons sequence more-sequences)
	    )
	  result
	  )))))


(defun-wcefi notany!! (predicate sequence &rest more-sequences)
  (simple-pvar-argument!! sequence &rest more-sequences)
  (safety-check
    (new-multiple-pvar-check more-sequences 'notany!!)
    )
  (without-void-sequence sequence notany!!
    (without-void-pvar-list more-sequences
      (let ((shortest-length (find-shortest-length sequence more-sequences)))
	(let!! ((result nil!!))
	  (declare (type boolean-pvar result))
	  (*map-vectors-nocheck
	    #'(lambda (&rest element-pvars)
		(*when (not!! result)
		  (declare (return-pvar-p nil))
		  (*set result (or!! result (the (pvar boolean) (apply predicate element-pvars))))
		  ))
	    (!! 0)
	    (!! shortest-length)
	    nil nil
	    (cons sequence more-sequences)
	    )
	  (*set result (not!! result))
	  result
	  )))))


(defun-wcefi notevery!! (predicate sequence &rest more-sequences)
  (simple-pvar-argument!! sequence &rest more-sequences)
  (safety-check
    (new-multiple-pvar-check more-sequences 'notevery!!)
    )
  (without-void-sequence sequence notevery!!
    (without-void-pvar-list more-sequences
      (let ((shortest-length (find-shortest-length sequence more-sequences)))
	(let!! ((result t!!))
	  (declare (type boolean-pvar result))
	  (*map-vectors-nocheck
	    #'(lambda (&rest element-pvars)
		(*when result
		  (declare (return-pvar-p nil))
		  (*set result (and!! result (the (pvar boolean) (apply predicate element-pvars))))
		  ))
	    (!! 0)
	    (!! shortest-length)
	    nil nil
	    (cons sequence more-sequences)
	    )
	  (*set result (not!! result))
	  result
	  )))))


(defun-wcefi reduce!!

	     (function sequence &key (from-end nil) (start (!! 0)) (end (length!! sequence)) (initial-value nil))  
  (simple-pvar-argument!! sequence &opt start end initial-value)
  (safety-check
    (new-two-pvar-check start end 'reduce!!)
    (if initial-value (new-vp-pvar-check initial-value 'reduce!!))
    )

  (without-void-sequence sequence reduce!!

    (without-void-pvars (start end)

      (let ((vlength (array-pvar-total-size sequence)))
    
	(when (zerop vlength)
	  (check-zero-length-sequence-start-and-end start end)
	  (return-from reduce!!
	    (if initial-value initial-value (funcall function))
	    ))

	(with-start-and-end-checked-and-coerced

	  start end vlength 'reduce!!

	  #'(lambda (start end)

	      (*locally
		(declare (type (field-pvar *) start end))

		(let!! (result)
		  (declare (type (pvar *) result))
		  nil
	
		  (*compile ()
	  
		    (*let ((reduction-length (-!! end start)))
		      (declare (type (pvar (unsigned-byte 32)) reduction-length))
		      (declare (return-pvar-p nil))
	    
		      ;; From CLtL, page 251
		      ;; If an :initial-value argument is given, it is logically
		      ;; placed before the subsequence (after it if :from-end is
		      ;; true) and included in the reduction operation.

		      (*cond
	      
			;; From CLtL, page 251.
			;; If the subsequence is empty and an :initial-value argument
			;; is given, then the :initial-value argument is returned and
			;; function is not called.  If no :initial-value is given, then
			;; the function is called with zero arguments.

			((zerop!! reduction-length)
			 (when (*or t!!)
			   (*nocompile
			     (if initial-value
				 (*set result initial-value)
				 (*set result (funcall function))
				 ))))
	      
			;; From CLtL, page 251
			;; If the subsequence contains exactly one element and no
			;; :initial-value is given, then that element is returned
			;; and function is not called.

			((=!! (!! 1) reduction-length)
			 (when (*or t!!)
			   (*nocompile
			     (if initial-value
				 (if from-end
				     (*set result (funcall function (aref!! sequence start) initial-value))
				     (*set result (funcall function initial-value (aref!! sequence start)))
				     )
				 (*set result (aref!! sequence start))
				 ))))
	      
			(t!!
		
			  (when (*or t!!)
		  
			    (if initial-value
				(*nocompile
				  (*set result initial-value)
				  (*map-vectors-nocheck
				    #'(lambda (element)
					(if from-end
					    (*set result (funcall function element result))
					    (*set result (funcall function result element))
					    ))
				    start end from-end nil sequence
				    ))
		      
				(*let ((first t!!))
				  (declare (type (pvar boolean) first))
				  (declare (return-pvar-p nil))
				  (*map-vectors-nocheck
				    #'(lambda (element)
					(*if first
					     (progn
					       (*nocompile (*set result element))
					       (*set first nil!!)
					       )
					     (*nocompile
					       (if from-end
						   (*set result (funcall function element result))
						   (*set result (funcall function result element))
						   ))))
				    start end from-end nil sequence
				    )))))
	      
			)))
	
		  result
	
		  ))))))))


(*defun *fill (sequence item &key (start (!! 0)) (end (length!! sequence)))
  
  (simple-pvar-argument!! (item start end))

  (safety-check
    (new-two-pvar-check start end '*fill)
    (new-vp-pvar-check item '*fill)
    )

  (without-void-sequence sequence *fill
    (without-void-pvars (item start end)
      (*fill-internal sequence item start end)
      )))



(defmacro check-test-test-not (test test-not)
  `(if (null ,test)
       (if (null ,test-not)
	   (setq ,test 'eql!!)
	   )
       (if ,test-not (error "You cannot specify :test and :test-not"))
       ))



(defun-wcefi internal-substitute!! (newitem olditem sequence start end from-end test test-not key count)
  
  (without-void-sequence-pvar sequence substitute!!

    (without-void-pvars (newitem start end)

      (let ((vlength (array-pvar-total-size sequence)))
    
	(when (zerop vlength)
	  (check-zero-length-sequence-start-and-end start end)
	  (return-from internal-substitute!! sequence)
	  )

	(with-start-and-end-checked-and-coerced 

	  start end vlength 'substitute!!

	  #'(lambda (start end)

	      (*compile ()
	
		(*let (found (number-substituted (!! 0)))
		  (declare (type (pvar boolean) found))
		  (declare (type (pvar (unsigned-byte 32)) number-substituted))
		  (declare (return-pvar-p nil))

		  (*map-vectors-nocheck
		    #'(lambda (element)
			(*set found nil!!)
			(if test
			    (if olditem
				(*set found (the (pvar boolean) (funcall test olditem (funcall key element))))
				(*set found (the (pvar boolean) (funcall test (funcall key element))))
				)
			    (if olditem
				(*set found (not!! (the (pvar boolean) (funcall test-not olditem (funcall key element)))))
				(*set found (not!! (the (pvar boolean) (funcall test-not (funcall key element)))))
				))
			(*when found
			  (declare (return-pvar-p nil))
			  (if count
			      (*nocompile
				(*when (<!! number-substituted count)
				  (declare (return-pvar-p nil))
				  (*set element newitem)
				  )
				(*compile () (*set number-substituted (1+!! number-substituted)))
				)
			      (*nocompile (*set element newitem))
			      )))
		    start end from-end nil sequence
		    )

		  )))))))
	  
  sequence

  )


(defun nsubstitute!!
       (newitem olditem sequence
	&key
	(from-end nil) (test nil) (test-not nil)
	(start (!! 0)) (end (length!! sequence))
	(count nil) (key #'identity)
	)
  (simple-pvar-argument!! newitem olditem &opt from-end start end count)
  (safety-check
    (sequence-function-pvar-args-check 'nsubstitute!! sequence newitem olditem start end)
    (if count (new-vp-pvar-check count 'nsubstitute!!))
    )
  (check-test-test-not test test-not)
  (internal-substitute!! newitem olditem sequence start end from-end test test-not key count)
  )

(defun nsubstitute-if!!
       (newitem test sequence
	&key (from-end nil) (start (!! 0)) (end (length!! sequence)) (key #'identity) (count nil)
	)
  (simple-pvar-argument!! newitem &opt from-end start end count)
  (safety-check
    (sequence-function-pvar-args-check 'nsubstitute-if!! sequence newitem start end)
    (if count (new-vp-pvar-check count 'nsubstitute-if!!))
    )
  (internal-substitute!! newitem nil sequence start end from-end test nil key count)
  )

(defun nsubstitute-if-not!!
       (newitem test sequence
	&key (from-end nil) (start (!! 0)) (end (length!! sequence)) (key #'identity) (count nil)
	)
  (simple-pvar-argument!! newitem &opt from-end start end count)
  (safety-check
    (sequence-function-pvar-args-check 'nsubstitute-if-not!! sequence newitem start end)
    (if count (new-vp-pvar-check count 'nsubstitute-if-not!!))
    )
  (internal-substitute!! newitem nil sequence start end from-end nil test key count)
  )


(defun substitute!!
       (newitem olditem sequence
	&key
	(from-end nil) (test nil) (test-not nil)
	(start (!! 0)) (end (length!! sequence))
	(count nil) (key #'identity)
	)
  (simple-pvar-argument!! sequence)
  (nsubstitute!! newitem olditem (copy-seq!! sequence)
		 :from-end from-end :test test :test-not test-not :start start :end end :count count :key key
		 ))

(defun substitute-if!!
       (newitem test sequence
	&key (from-end nil) (start (!! 0)) (end (length!! sequence)) (key #'identity) (count nil)
	)
  (simple-pvar-argument!! sequence)
  (nsubstitute-if!! newitem test (copy-seq!! sequence)
		    :from-end from-end :start start :end end :count count :key key
		    ))

(defun substitute-if-not!!
       (newitem test sequence
	&key (from-end nil) (start (!! 0)) (end (length!! sequence)) (key #'identity) (count nil)
	)
  (simple-pvar-argument!! sequence)
  (nsubstitute-if-not!! newitem test (copy-seq!! sequence)
			:from-end from-end :start start :end end :count count :key key
			))



(defun-wcefi internal-find!! (sequence start end item from-end test test-not key return-value-if-not-found)
  
  (without-void-sequence-pvar sequence find!!

    (without-void-pvars (start end)

      (let ((vlength (array-pvar-total-size sequence)))
    
	(when (zerop vlength)
	  (check-zero-length-sequence-start-and-end start end)
	  (return-from internal-find!! (if return-value-if-not-found return-value-if-not-found nil!!))
	  )

	(with-start-and-end-checked-and-coerced

	  start end vlength 'find!!

	  #'(lambda (start end)

	      (*compile ()
	
		(let!! ((found nil!!))
		  (declare (type (pvar boolean) found))
	  
		  (*let (result)
	    
		    (*map-vectors-nocheck
		      #'(lambda (element)
			  (*when (not!! found)
			    (if test
				(if item
				    (*set found (the (pvar boolean) (funcall test item (funcall key element))))
				    (*set found (the (pvar boolean) (funcall test (funcall key element))))
				    )
				(if item
				    (*set found (not!! (the (pvar boolean) (funcall test-not item (funcall key element)))))
				    (*set found (not!! (the (pvar boolean) (funcall test-not (funcall key element)))))
				    ))
			    (*when found (*nocompile (*set result element)))
			    ))
		      start end from-end nil sequence
		      )
	    
		    (if (*and found)
			result
			(*nocompile
			  (if return-value-if-not-found
			      (if!! found result return-value-if-not-found)
			      (if!! found result nil!!)
			      )))
	    
		    )))))))))


(defun find!! (item sequence
	       &key
	       (from-end nil) (test nil) (test-not nil)
	       (start (!! 0)) (end (length!! sequence))
	       (key #'identity) (return-value-if-not-found nil)
	       )
  (simple-pvar-argument!! item sequence &opt from-end start end return-value-if-not-found)
  (safety-check
    (sequence-function-pvar-args-check 'find!! sequence item start end)
    (if return-value-if-not-found (new-vp-pvar-check return-value-if-not-found 'find!!))
    )
  (check-test-test-not test test-not)
  (internal-find!! sequence start end item from-end test test-not key return-value-if-not-found)
  )

(defun find-if!! (test sequence
		  &key (from-end nil) (start (!! 0)) (end (length!! sequence)) (key #'identity) (return-value-if-not-found nil))
  (simple-pvar-argument!! sequence &opt from-end start end return-value-if-not-found)
  (safety-check
    (sequence-function-pvar-args-check 'find-if!! sequence start end)
    (if return-value-if-not-found (new-vp-pvar-check return-value-if-not-found 'find-if!!))
    )
  (internal-find!! sequence start end nil from-end test nil key return-value-if-not-found)
  )

(defun find-if-not!!
       (test sequence
	&key
	(from-end nil) (start (!! 0)) (end (length!! sequence))
	(key #'identity) (return-value-if-not-found nil)
	)
  (simple-pvar-argument!! sequence &opt from-end start end return-value-if-not-found)
  (safety-check
    (sequence-function-pvar-args-check 'find-if-not!! sequence start end)
    (if return-value-if-not-found (new-vp-pvar-check return-value-if-not-found 'find-if-not!!!!))
    )
  (internal-find!! sequence start end nil from-end nil test key return-value-if-not-found)
  )


(*proclaim '(ftype (function (t t &rest t) (pvar (signed-byte *))) position!! position-if!! position-if-not!!))
(*proclaim '(ftype (function (t t &rest t) (pvar (unsigned-byte *))) count!! count-if!! count-if-not!!))


;;; Position!! returns -1 in processors in which the
;;; item was not found.  This is in violation of CLtL,
;;; which has position return NIL.  Tough cookies.


(defun-wcefi internal-position!! (sequence start end item from-end test test-not key)

  (without-void-sequence-pvar sequence position!!

    (without-void-pvars (start end)

      (let ((vlength (array-pvar-total-size sequence)))
    
	(when (zerop vlength)
	  (check-zero-length-sequence-start-and-end start end)
	  (return-from internal-position!! (!! -1))
	  )

	(with-start-and-end-checked-and-coerced

	  start end vlength 'position!!

	  #'(lambda (start end)

	      (*compile ()
	
		(let!! (position (found nil!!))
		  (declare (type (pvar boolean) found))
		  (declare (type (pvar (signed-byte 32)) position))
	  
		  (*map-vectors-nocheck
		    #'(lambda (index element)
			(*when (not!! found)
			  (if test
			      (if item
				  (*set found (the (pvar boolean) (funcall test item (funcall key element))))
				  (*set found (the (pvar boolean) (funcall test (funcall key element))))
				  )
			      (if item
				  (*set found (not!! (the (pvar boolean) (funcall test-not item (funcall key element)))))
				  (*set found (not!! (the (pvar boolean) (funcall test-not (funcall key element)))))
				  ))
			  (*when found (*set position (!! (the fixnum index))))
			  ))
		    start end from-end t sequence
		    )
	  
		  (*when (not!! found) (declare (return-pvar-p nil)) (*set position (!! -1)))
	  
		  position
	  
		  ))))))))


(defun position!! (item sequence
		   &key (from-end nil) (test nil) (test-not nil) (start (!! 0)) (end (length!! sequence)) (key #'identity)
		   )
  (simple-pvar-argument!! item sequence &opt from-end start end)
  (safety-check
    (sequence-function-pvar-args-check 'position!! sequence item start end)
    )
  (check-test-test-not test test-not)
  (internal-position!! sequence start end item from-end test test-not key)
  )

(defun position-if!! (test sequence &key (from-end nil) (start (!! 0)) (end (length!! sequence)) (key #'identity))
  (simple-pvar-argument!! sequence &opt from-end start end)
  (safety-check
    (sequence-function-pvar-args-check 'position-if!! sequence start end)
    )
  (internal-position!! sequence start end nil from-end test nil key)
  )

(defun position-if-not!! (test sequence &key (from-end nil) (start (!! 0)) (end (length!! sequence)) (key #'identity))
  (simple-pvar-argument!! sequence &opt from-end start end)
  (safety-check
    (sequence-function-pvar-args-check 'position-if-not!! sequence start end)
    )
  (internal-position!! sequence start end nil from-end nil test key)
  )



(defun-wcefi internal-count!! (sequence start end item test test-not key)

  (without-void-sequence-pvar sequence count!!

    (without-void-pvars (start end)

      (let ((vlength (array-pvar-total-size sequence)))
    
	(when (zerop vlength)
	  (check-zero-length-sequence-start-and-end start end)
	  (return-from internal-count!! (!! 0))
	  )

	(with-start-and-end-checked-and-coerced

	  start end vlength 'count!!

	  #'(lambda (start end)

	      (*compile ()
	
		(let!! ((count (!! 0)) found)
		  (declare (type (pvar (unsigned-byte 32)) count))
		  (declare (type (pvar boolean) found))
		  (*map-vectors-nocheck
		    #'(lambda (element)
			(if test
			    (if item
				(*set found (the (pvar boolean) (funcall test item (funcall key element))))
				(*set found (the (pvar boolean) (funcall test (funcall key element))))
				)
			    (if item
				(*set found (not!! (the (pvar boolean) (funcall test-not item (funcall key element)))))
				(*set found (not!! (the (pvar boolean) (funcall test-not (funcall key element)))))
				))
			(*when found (declare (return-pvar-p nil)) (*set count (+!! (!! 1) count)))
			)
		    start end nil nil sequence
		    )
		  count
		  ))))))))


(defun count!! (item sequence
		&key (from-end nil) (test nil) (test-not nil) (start (!! 0)) (end (length!! sequence)) (key #'identity)
		)
  (declare (ignore from-end))
  (simple-pvar-argument!! item sequence
			  ; from-end  ;;; Uncomment when no longer ignored
			  start end)
  (safety-check
    (sequence-function-pvar-args-check 'count!! sequence item start end)
    )
  (check-test-test-not test test-not)
  (internal-count!! sequence start end item test test-not key)
  )
  
(defun count-if!! (test sequence &key (from-end nil) (start (!! 0)) (end (length!! sequence)) (key #'identity))
  (declare (ignore from-end))
  (simple-pvar-argument!! sequence
			  ; from-end  ;;; Uncomment when no longer ignored
			  start end)
  (safety-check (sequence-function-pvar-args-check 'count-if!! sequence start end))
  (internal-count!! sequence start end nil test nil key)
  )

(defun count-if-not!! (test sequence &key (from-end nil) (start (!! 0)) (end (length!! sequence)) (key #'identity))
  (declare (ignore from-end))
  (simple-pvar-argument!! sequence
			  ; from-end  ;;; Uncomment when no longer ignored
			  start end)
  (sequence-function-pvar-args-check 'count-if-not!! sequence start end)
  (internal-count!! sequence start end nil nil test key)
  )



;;; Tests for the sequence functions.


(defun-wco test-sequence-functions ()
  
  (*nocompile
    
    (flet
      ((assert-equalp (a1 a2 tag)
	 (let ((*print-array* t))
	   (assert (*and (equalp!! a1 a2)) ()
		   "The arrays for tag ~S are not equalp.  (PREF A1 0): ~A, (PREF A2 0): ~A"
		   tag (pref a1 0) (pref a2 0)
		   )))
       (assert-true (pvar tag)
	 (assert (*and pvar) () "The pvar for tag ~S is not everywhere true." tag)
	 )
       (note (name) (format t "~S, " name))
       )
      
      (macrolet
	((test-equalp (a1 a2) `(*let () (assert-equalp ,a1 ,a2 ',(car a1))))
	 (test-true (x) `(*let () (assert-true ,x ',(car x))))
	 )
	
	(*let ((sequence (!! '#(0 1 2 3 4 5))))
	  (declare (type (pvar (array (unsigned-byte 8) (6))) sequence))
	  
	  (format t "~%Testing sequence functions.~%")
	  
	  ;; subseq
	  
	  (test-equalp (subseq!! sequence (!! 1)) (!! '#(1 2 3 4 5)))
	  (test-equalp (subseq!! sequence (!! 0) (!! 2)) (!! '#(0 1)))
	  (test-equalp (subseq!! sequence (!! 2) (!! 5)) (!! '#(2 3 4)))
	  
	  (note 'subseq!!)
	  
	  ;; copy-seq
	  
	  (test-equalp (copy-seq!! sequence) sequence)
	  
	  (note 'copy-seq!!)
	  
	  ;; reverse
	  
	  (test-equalp (reverse!! sequence) (!! '#(5 4 3 2 1 0)))
	  (test-equalp (reverse!! (subseq!! sequence (!! 3))) (!! '#(5 4 3)))
	  (test-equalp (reverse!! (subseq!! sequence (!! 2) (!! 3))) (!! '#(2)))
	  
	  (note 'reverse!!)
	  
	  ;; some
	  
	  (test-true (some!! 'numberp!! sequence))
	  (test-true (some!! 'oddp!! sequence))
	  (test-true (some!! #'(lambda (x) (=!! x (!! 4))) sequence))
	  
	  (note 'some!!)
	  
	  ;; every
	  
	  (test-true (every!! 'numberp!! sequence))
	  (test-true (every!! 'plusp!! (subseq!! sequence (!! 1))))
	  (test-true (every!! #'(lambda (x) (<!! x (!! 20))) sequence))
	  
	  (note 'every!!)
	  
	  ;; notany
	  
	  (test-true (notany!! 'characterp!! sequence))
	  (test-true (notany!! 'minusp!! sequence))
	  (test-true (notany!! #'(lambda (x) (>!! x (!! 20))) sequence))
	  
	  (note 'notany!!)
	  
	  ;; notevery
	  
	  (test-true (notevery!! 'oddp!! sequence))
	  (test-true (notevery!! 'plusp!! sequence))
	  (test-true (notevery!! #'(lambda (x) (=!! x (!! 3))) sequence))
	  
	  (note 'notevery!!)
	  
	  ;; reduce
	  
	  (test-true (=!! (!! 0) (reduce!! 'min!! sequence :initial-value (!! 20))))
	  (test-true (=!! (!! 3) (reduce!! '+!! sequence :start (!! 0) :end (!! 3))))
	  (test-true (=!! (!! 24) (reduce!! '*!! sequence :start (!! 2) :end (!! 5))))
	  (test-true (=!! (!! -15) (reduce!! '-!! sequence)))
	  (test-true (=!! (!! -3) (reduce!! '-!! sequence :from-end t)))
	  
	  (note 'reduce!!)
	  
	  ;; *fill
	  
	  (*fill sequence (!! 0))
	  (test-true (every!! 'zerop!! sequence))
	  (*fill sequence (!! 2) :start (!! 1) :end (!! 3))
	  (test-true (every!! #'(lambda (x) (=!! x (!! 2))) (subseq!! sequence (!! 1) (!! 3))))
	  (*set sequence (!! '#(0 1 2 3 4 5)))
	  
	  (note '*fill)
	  
	  ;; substitute
	  
	  (test-equalp (substitute!! (!! 8) (!! 0) sequence) (!! '#(8 1 2 3 4 5)))
	  (test-equalp (substitute!! (!! 8) (!! 3) sequence :test '>!!) (!! '#(8 8 8 3 4 5)))
	  (test-equalp (substitute-if!! (!! 8) 'oddp!! sequence :start (!! 2)) (!! '#(0 1 2 8 4 8)))
	  (test-equalp (substitute-if-not!! (!! 8) #'(lambda (x) (not!! (plusp!! x))) sequence :end (!! 3)) (!! '#(0 8 8 3 4 5)))
	  
	  (note 'substitute!!)
	  
	  ;; find
	  
	  (test-true (=!! (find!! t!! sequence :key #'oddp!!) (!! 1)))
	  (test-true (=!! (find!! t!! sequence :key #'oddp!! :from-end t) (!! 5)))
	  (test-true (=!! (find-if!! #'oddp!! sequence :start (!! 2)) (!! 3)))
	  (test-true (=!! (find-if!! #'oddp!! sequence :from-end t :end (!! 4)) (!! 3)))
	  
	  (note 'find!!)
	  
	  ;; position
	  
	  (test-true (=!! (position!! (!! 3) sequence) (!! 3)))
	  (test-true (=!! (position!! t!! sequence :key #'oddp!!) (!! 1)))
	  (test-true (=!! (position-if!! #'oddp!! sequence :from-end t) (!! 5)))
	  (test-true (=!! (position-if!! #'minusp!! sequence :from-end t :key #'(lambda (x) (-!! x (!! 3)))) (!! 2)))
	  
	  (note 'position!!)
	  
	  ;; count
	  
	  (test-true (=!! (count!! (!! 2) sequence) (!! 1)))
	  (test-true (=!! (count!! (!! -1) sequence) (!! 0)))
	  (test-true (=!! (count-if!! #'oddp!! sequence) (!! 3)))
	  (test-true (=!! (count-if-not!! #'zerop!! sequence :start (!! 2) :end (!! 4) :from-end t :key #'1-!!) (!! 2)))
	  
	  (note 'count!!)
	  
	  (format t "~%Finished simple sequence tests~%")
	  
	  )))))


