;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-I; MUSER: YES -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;; #+*LISP-HARDWARE
;;; (import '(proclaimed-type proclaimed-ftype proclaimed-declaration-p proclaimed-special-p check-paris-restriction)
;;; 	(find-package 'slc)
;;; 	)
;;; 
;;; #+*LISP-HARDWARE
;;; (proclaim '(special slc::*compiler-options* slc::*slc-function-types*))
;;; 
;;; 
;;; 
;;; #+*LISP-HARDWARE
;;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;;   (setf (symbol-function 'slc::canonical-type) 'canonical-type)
;;;   )


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *maximum-integer-length* #+*LISP-HARDWARE cm:*maximum-integer-length* #+*LISP-SIMULATOR 128)
  (defvar *maximum-signficand-length* #+*LISP-HARDWARE cm:*maximum-significand-length* #+*LISP-SIMULATOR 96)
  (defvar *maximum-exponent-length* #+*LISP-HARDWARE cm:*maximum-exponent-length* #+*LISP-SIMULATOR 32)
  )


#+*LISP-HARDWARE
(eval-when (load eval compile)
  (defun *defun-maybe (function-name arguments)
    `(let* ((old-*temp-pvar-list* *temp-pvar-list*)
	    (place-holder (cm:allocate-stack-field 0)))
       (maybe-return-pvar place-holder (,function-name ,@arguments) old-*temp-pvar-list*)))

  (defun *defun-yes (function-name arguments)
    `(let ((place-holder (cm:allocate-stack-field 0)))
       (values (move-pvar-to-place-holder place-holder (,function-name ,@arguments)))))

  (defun *defun-no (function-name arguments)
    `(let ((*lisp-i::*temp-pvar-list* *lisp-i::*temp-pvar-list*)
	   (*lisp-i::place-holder (cm:allocate-stack-field 0)))
       (prog1 (,function-name ,@arguments)
	      (cm:deallocate-upto-stack-field *lisp-i::place-holder))))

  (defun *defun-type (function)
    (let* ((ftype (proclaimed-ftype function)) (return-type (if ftype (caddr ftype))))
      (if ftype
	  ;; return type must be canonical, (which it is supposed to be
	  (if (or (eq return-type 'pvar)
		  (and (consp return-type) (eq (car return-type) 'pvar)))
	      :yes 
	      (if (and (consp return-type) (member (car return-type) '(satisfies and or not member values)))
		  :dont-know
		  :no))
	  :dont-know)))

  (defun proclaim-*defun-1 (function-name)
    (let ((old-source-file-name (get function-name :source-file-name)))	
      (setf (*lisp-i::get-*defun-function function-name) (make-*defun-function function-name))
      (setf (macro-function function-name)
	    #'(lambda (%form% %environment% &aux (args (cdr %form%)))
		(or (funcall '*lisp-compiler-hook %form% %environment%)
		    (case (*defun-type function-name)
		      (:dont-know (*defun-maybe (make-*defun-function function-name) args))
		      (:no (*defun-no (make-*defun-function function-name) args))
		      (:yes (*defun-yes (make-*defun-function function-name) args))))))
      (setf (get function-name :source-file-name) old-source-file-name))))


#+*LISP-SIMULATOR
(eval-when (load eval compile)
  (defun proclaim-*defun-1 (function-name)
    (when (not (macro-function function-name))
      (let ((old-temp-pvar-list-symbol (gensym "OLD-TEMP-PVAR-LIST-")))
	(setf (macro-function function-name)
	      #'(lambda (form environment)
		  (declare (ignore environment))
		  `(let ((,old-temp-pvar-list-symbol *temp-pvar-list*))
		     (handle-returning-pvar
		       (,(make-*defun-function function-name) ,@(cdr form))
		       ,old-temp-pvar-list-symbol
		       nil
		       )))))))
  )




(defmacro proclaim-all-*defuns ()
  `(progn
     ,@(mapcan
	 #'(lambda (symbol)
	     (let ((type (get symbol :starlisp-type)))
	       (when (eq type :*defun)
		 `((*proclaim '(*defun ,symbol)))
		 )))
	 *all-external-symbols*
	 )))


(proclaim-all-*defuns)


