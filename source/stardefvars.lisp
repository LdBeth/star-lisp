;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-i; MUSER: YES-*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;;  *DEFVARS

(*proclaim '(type boolean-pvar nil!! t!!))


;;; *PROCLAIM-*DEFVAR
;;; *DEFVAR


(defmacro *proclaim-*defvar (variable pvar-type &key initial-value documentation vp-set)
  `(progn
     (*proclaim '(type ,pvar-type) ,variable)
     (*defvar ,variable ,initial-value ,documentation ,vp-set)
     ))


(defmacro *defvar (variable &optional initial-value documentation (vp-set '*default-vp-set*))
  (if (or (member variable '(t!! nil!!)) (constantp variable))
      (error "you cannot redefine ~a, it is a constant." variable))
  (let ((type (get variable :*lisp-type)))
    #+*lisp-simulator (declare (ignorable type))
    (if (and #+*LISP-SIMULATOR nil
	     type
	     (not (equal type '(pvar *)))
	     (not (equal type '(pvar t)))
	     (consp type)
	     (eq (car type) 'pvar)
	     (or *compilep* (null initial-value)))
	;; *proclaim has been done, expand slightly differently.
	(let ((init (gentemp (format nil "INIT-~:@(~A~)-" variable) (symbol-package variable))))
	  `(progn
	     (proclaim '(special ,variable))
	     (setf (documentation ',variable 'variable) ',documentation)
	     #+*LISP-HARDWARE
	     (eval-when (compile eval load) (pushnew ',variable slc::*distinct-pvars*))
	     (defun ,init ()
	       ,(if (eq vp-set '*default-vp-set*)
		    `(*all (setq ,variable (allocate!! ,initial-value ',variable ',type t)) nil)
		    `(*with-vp-set ,vp-set
		       (*all (setq ,variable (allocate!! ,initial-value ',variable ',type t)) nil))))
	     (eval-when (:load-toplevel :execute)
	       (*defvar-1 ',variable ',initial-value ',vp-set ',init ',type))
	     ',variable))
	`(progn
	   (proclaim '(special ,variable))
	   (setf (documentation ',variable 'variable) ',documentation)
	   #+*LISP-HARDWARE
	   (eval-when (:compile-toplevel :load-toplevel :execute) (pushnew ',variable slc::*distinct-pvars*))
	   (eval-when (:load-toplevel :execute) (*defvar-1 ',variable ',initial-value ',vp-set))
	   ',variable))))


;;; (defun *defvar-1 (variable &optional initial-value (vp-set-name '*default-vp-set*) init-function type)
;;; 
;;;   ;; Add the *DEFVAR onto our list of all *DEFVARS,
;;;   ;; and if we have *COLD-BOOT'ed, try to actually
;;;   ;; allocate this *DEFVAR and give it its initial
;;;   ;; value.  Don't try to allocate it if the VP SET
;;;   ;; it belongs to has not been instantiated yet.
;;; 
;;;   (when (eq vp-set-name '*current-vp-set*) (setq vp-set-name (vp-set-name *current-vp-set*)))
;;;   (when (or (not (symbolp vp-set-name))
;;; 	    (not (boundp vp-set-name))
;;; 	    (not (vp-set-p (symbol-value vp-set-name)))
;;; 	    (null (vp-set-name (symbol-value vp-set-name)))
;;; 	    (null (or (eq vp-set-name '*default-vp-set*) (find vp-set-name *all-def-vp-sets* :test #'eq :key #'vp-set-name))))
;;;     (cerror "Delete the old *defvar definition if any, and return from *defvar"
;;; 	    (cond ((not (symbolp vp-set-name)) "The object ~S is not the name of a Vp Set")
;;; 		  ((not (boundp vp-set-name)) "The symbol ~S, which is supposed to have a Vp Set as its value, is unbound")
;;; 		  ((not (vp-set-p (symbol-value vp-set-name))) "The symbol ~S has value ~S, which is not a Vp Set")
;;; 		  ((null (vp-set-name (symbol-value vp-set-name)))
;;; 		   "The symbol ~S is bound to a Vp Set, ~S, which was not defined using DEF-VP-SET~@
;;;                     but rather probably with CREATE-VP-SET.  You can only define *DEFVAR's in Vp Sets~@
;;;                     defined with DEF-VP-SET.")
;;; 		  (t "The Vp Set named ~S is not currently defined.  You may have deleted it~@
;;;                       from *Lisp's knowledge by using :undefine-all t in *cold-boot.  You~@
;;;                       may wish to re-evaluate its definition."))
;;; 	    vp-set-name (if (and (symbolp vp-set-name) (boundp vp-set-name)) (symbol-value vp-set-name)))
;;;     (delete-old-*defvar-definition variable)
;;;     (return-from *defvar-1 nil))
;;; 
;;;   (let ((new-*defvar-specification
;;; 	  (make-*defvar-specification
;;; 	    :name variable :initial-value-form initial-value :vp-set-name vp-set-name :in-vp-set-definition-p nil
;;; 	    :initial-value-function init-function :proclaimed-type type))
;;; 	(old-*defvar-specification-index
;;; 	  (position variable *all-*defvar-specifications* :test #'eq :key #'*defvar-specification-name))
;;; 	(vp-set (symbol-value vp-set-name)))
;;;     (*deallocate-*defvar-if-possible variable)
;;;     (when (and (*lisp-runnable-p) (vp-set-instantiated-p (symbol-value vp-set-name)))
;;;       (if init-function
;;; 	  (funcall (if (not (compiled-function-p (symbol-function init-function))) (compile init-function) init-function))
;;; 	  (allocate-*defvar variable initial-value vp-set)))
;;;     (if old-*defvar-specification-index
;;; 	(setf (nth old-*defvar-specification-index *all-*defvar-specifications*) new-*defvar-specification)
;;; 	(setq *all-*defvar-specifications* (nconc *all-*defvar-specifications* (list new-*defvar-specification)))))
;;;   (values))

;;; (defun delete-old-*defvar-definition (name)
;;;   (let ((specification (find name *all-*defvar-specifications* :key #'*defvar-specification-name)))
;;;     (if (and specification (*defvar-specification-initial-value-function specification))
;;; 	(fmakunbound (*defvar-specification-initial-value-function specification))))
;;;   (setq *all-*defvar-specifications*
;;; 	(delete name *all-*defvar-specifications* :test #'eq :key #'*defvar-specification-name))
;;;   (*deallocate-*defvar-if-possible name)
;;;   (makunbound name))
;;; 

;;; (defun allocate-*defvar (symbol &optional initial-value vp-set &aux return-pvar)
;;;   ;; Allocate a pvar, and make the symbol naming the
;;;   ;; *DEFVAR have as its value that pvar.
;;;   (setq return-pvar (allocate-*defvar-1 symbol initial-value vp-set))
;;;   (setf (pvar-name return-pvar) symbol)
;;;   (setf (symbol-value symbol) return-pvar)
;;;   return-pvar)
;;; 


(defvar
  smashed-location2
  "This pvar structure was allocated with ALLOCATE!! but was reclaimed by a *COLD-BOOT.  You should not access it."
  )

(defun *defvar-*cold-boot-initialization ()

  (dolist (pvar *all-allocate!!-pvars*)
    (setf (pvar-location pvar) smashed-location2)
    (set-pvar-length pvar smashed-location2)
    (setf (pvar-type pvar) smashed-location2))

  (setq *all-allocate!!-pvars* nil)

  ;; now must recreate all *DEFVARS.  
  (allocate-*defvars-for-*cold-boot))


(defun allocate-*defvars-for-*cold-boot ()
  (let ((bad-*defvar-specifications nil))
    (mapc
      #'(lambda (*defvar-specification)
	  (let* ((pvar-symbol (*defvar-specification-name *defvar-specification))
		 (pvar-initial-value-form (*defvar-specification-initial-value-form *defvar-specification))
		 (pvar-vp-set-name (*defvar-specification-vp-set-name *defvar-specification))
		 (initial-value-function (*defvar-specification-initial-value-function *defvar-specification))
		 (proclaimed-type (*defvar-specification-proclaimed-type *defvar-specification))
		 (current-type (if initial-value-function (get pvar-symbol 'type))))
	    (when (or (not (boundp pvar-vp-set-name))
		      (not (symbolp pvar-vp-set-name))
		      (not (vp-set-p (symbol-value pvar-vp-set-name)))
		      (and (not (eq pvar-vp-set-name '*default-vp-set*))
			   (null (position pvar-vp-set-name *all-def-vp-sets* :test #'eq :key #'vp-set-name))))
	      (cerror "Blow away the *defvar definition and continue initializing other *defvars"
		      "The *defvar ~S has a Vp Set named ~S, which is not now an existing Vp Set defined with DEF-VP-SET."
		      pvar-symbol pvar-vp-set-name)
	      (push *defvar-specification bad-*defvar-specifications))
	    #+*LISP-SIMULATOR
	    (progn proclaimed-type current-type)
	    #+*LISP-HARDWARE
	    (when initial-value-function
	      (unless (equal proclaimed-type current-type)
		(warn "The proclaimed type for ~S has been changed since the *defvar ~S was evaluated.~%~@
                       The initialization function is being recompiled."
		      pvar-symbol pvar-symbol)
		(compile
		  (eval
		    `(defun ,initial-value-function () 
		       ,(if (eq pvar-vp-set-name '*default-vp-set*)
			    `(*all (setq ,pvar-symbol
					 (allocate!! ,pvar-initial-value-form ',pvar-symbol ',current-type t))
				   nil)
			    `(*with-vp-set ,pvar-vp-set-name
			       (*all (setq ,pvar-symbol
					   (allocate!! ,pvar-initial-value-form ',pvar-symbol ',current-type t))
				     nil))))))
		(setf (*defvar-specification-proclaimed-type *defvar-specification) current-type)))
	    ;; JP 5/9/89.  Make *defvars in non-instantiated Vp Sets be unbound
	    (if (vp-set-instantiated (symbol-value pvar-vp-set-name))
		(allocate-*defvar-with-errors-trapped
		  pvar-symbol pvar-initial-value-form (symbol-value pvar-vp-set-name) initial-value-function
		  )
		(makunbound pvar-symbol)
		)))
;		(when (vp-set-instantiated (symbol-value pvar-vp-set-name))
;		  (allocate-*defvar-with-errors-trapped
;		    pvar-symbol pvar-initial-value-form (symbol-value pvar-vp-set-name) initial-value-function))))
      *all-*defvar-specifications*)
    (mapc #'(lambda (*defvar-specification)
	      (delete-old-*defvar-definition (*defvar-specification-name *defvar-specification)))
	  bad-*defvar-specifications)))



(defun *defvar-pvar-p (pvar)
  (dolist (spec *all-*defvar-specifications*)
    (let ((pvar-symbol (*defvar-specification-name spec)))
      (when (and (boundp pvar-symbol) (eq pvar (symbol-value pvar-symbol)))
	(return-from *defvar-pvar-p t)
	)))
  nil
  )

(defun allocated-pvar-p (pvar)
  ;;(simple-pvar-argument!! pvar)
  (if (or (*defvar-pvar-p pvar)
          (member pvar *all-allocate!!-pvars* :test #'eq)
          )
      :heap
    (dolist (stack-pvar *temp-pvar-original-list*)
      (if (eq stack-pvar pvar)
          (return :stack)
        (if (eq stack-pvar (car *temp-pvar-list*)) (return nil))
        ))))


#+*LISP-SIMULATOR
(defun allocate-*defvar-with-errors-trapped (pvar-symbol pvar-initial-value-form pvar-vp-set init-function)
  (declare (ignore init-function))
  (with-all-errors-trapped
    (allocate-*defvar pvar-symbol pvar-initial-value-form pvar-vp-set)
    (progn
      (format t "An error occurred while evaluating ~S, the initial value for for the *defvar named ~S~%"
	      pvar-initial-value-form pvar-symbol
	      )
      (format t "Your options are to skip allocating the *defvar this time and continue, ~@
                           or to skip the allocation and destroy the *defvar definition and continue, ~@
                           or to Abort and try to fix the problem"
	      )
      (cond
	((y-or-n-p "Skip allocating the *defvar and continue? ")
	 (makunbound pvar-symbol)
	 )
	((y-or-n-p "Skip allocation, destroy the *defvar and continue? ")
	 (makunbound pvar-symbol)
	 (setq *all-*defvar-specifications*
	       (delete pvar-symbol *all-*defvar-specifications* :test #'eq :key #'*defvar-specification-name)
	       ))
	(t (error "You will have to abort to top level"))
	))))


