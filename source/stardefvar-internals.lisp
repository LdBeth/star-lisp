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


(defun allocate-*defvar (symbol &optional initial-value vp-set &aux return-pvar)
  ;; Allocate a pvar, and make the symbol naming the
  ;; *DEFVAR have as its value that pvar.
  (setq return-pvar (allocate-*defvar-1 symbol initial-value vp-set))
  (setf (pvar-name return-pvar) symbol)
  (setf (symbol-value symbol) return-pvar)
  return-pvar)

(defun allocate-*defvar-1 (symbol initial-value vp-set)
  ;; Turn on the VP SET to which this *defvar belongs,
  ;; and use ALLOCATE!! to create an initialize a pvar
  ;; which will be the value of this *DEFVAR.
  (let ((canonical-pvar-type (or (get symbol 'type) '(pvar *)))
	return-pvar)
    (let ((canonical-pvar-type-with-integer-sizes
	    (non-lexical-canonical-pvar-type-with-numeric-lengths-from-canonical-pvar-type canonical-pvar-type)))
      (*with-vp-set vp-set
	(*all
	  (setq return-pvar (allocate!! (eval initial-value) nil canonical-pvar-type-with-integer-sizes))
	  (setq *all-allocate!!-pvars* (delete return-pvar *all-allocate!!-pvars* :test #'eq))))
      return-pvar)))



(defun *defvar-1 (variable &optional initial-value (vp-set-name '*default-vp-set*) init-function type)

  ;; Add the *DEFVAR onto our list of all *DEFVARS,
  ;; and if we have *COLD-BOOT'ed, try to actually
  ;; allocate this *DEFVAR and give it its initial
  ;; value.  Don't try to allocate it if the VP SET
  ;; it belongs to has not been instantiated yet.

  (when (eq vp-set-name '*current-vp-set*) (setq vp-set-name (vp-set-name *current-vp-set*)))
  (when (or (not (symbolp vp-set-name))
	    (not (boundp vp-set-name))
	    (not (vp-set-p (symbol-value vp-set-name)))
	    (null (vp-set-name (symbol-value vp-set-name)))
	    (null (or (eq vp-set-name '*default-vp-set*) (find vp-set-name *all-def-vp-sets* :test #'eq :key #'vp-set-name))))
    (cerror "Delete the old *defvar definition if any, and return from *defvar"
	    (cond ((not (symbolp vp-set-name)) "The object ~S is not the name of a Vp Set")
		  ((not (boundp vp-set-name)) "The symbol ~S, which is supposed to have a Vp Set as its value, is unbound")
		  ((not (vp-set-p (symbol-value vp-set-name))) "The symbol ~S has value ~S, which is not a Vp Set")
		  ((null (vp-set-name (symbol-value vp-set-name)))
		   "The symbol ~S is bound to a Vp Set, ~S, which was not defined using DEF-VP-SET~@
                    but rather probably with CREATE-VP-SET.  You can only define *DEFVAR's in Vp Sets~@
                    defined with DEF-VP-SET.")
		  (t "The Vp Set named ~S is not currently defined.  You may have deleted it~@
                      from *Lisp's knowledge by using :undefine-all t in *cold-boot.  You~@
                      may wish to re-evaluate its definition."))
	    vp-set-name (if (and (symbolp vp-set-name) (boundp vp-set-name)) (symbol-value vp-set-name)))
    (delete-old-*defvar-definition variable)
    (return-from *defvar-1 nil))

  (let ((new-*defvar-specification
	  (make-*defvar-specification
	    :name variable :initial-value-form initial-value :vp-set-name vp-set-name :in-vp-set-definition-p nil
	    :initial-value-function init-function :proclaimed-type type))
	(old-*defvar-specification-index
	  (position variable *all-*defvar-specifications* :test #'eq :key #'*defvar-specification-name))
	(vp-set (symbol-value vp-set-name)))
    (*deallocate-*defvar-if-possible variable)
    (when (and (*lisp-runnable-p) (vp-set-instantiated-p (symbol-value vp-set-name)))
      (if init-function
	  (funcall (if (not (compiled-function-p (symbol-function init-function))) (compile init-function) init-function))
	  (allocate-*defvar variable initial-value vp-set)))
    (if old-*defvar-specification-index
	(setf (nth old-*defvar-specification-index *all-*defvar-specifications*) new-*defvar-specification)
	(setq *all-*defvar-specifications* (nconc *all-*defvar-specifications* (list new-*defvar-specification)))))
  (values))


;;; *DEALLOCATE
;;; *DEALLOCATE-*DEFVARS

;;;; DEALLOCATION OF PVARS


(defun *deallocate (pvar)

  (new-pvar-check pvar '*deallocate)

  (when (not (member pvar *all-allocate!!-pvars* :test #'eq))
    (cerror
      "Return from *deallocate without doing anything"
      "You are trying to use *deallocate on a pvar, ~S, that was not allocated using allocate!!,~@
       or has already been deallocated.  If you want to remove a pvar defined with *defvar~@
       use the *deallocate-*defvars function.
      "
      pvar
      )
    (return-from *deallocate nil)
    )

  (*deallocate-internal pvar :allocate!!)

  )

 
(defun *deallocate-*defvars (&rest pvar-names)

  (when (or (equal pvar-names '(:all)) (equal pvar-names '(:all-noconfirm)))
    (if (equal pvar-names '(:all))
	(cerror "Do it anyway" "Deleting all *DEFVARs may cause a library package to fail.~@
                            If you are certain you want to do this, hit the Resume key."))
    (setq pvar-names (mapcar #'*defvar-specification-name *all-*defvar-specifications*))
    )

  (assert (every #'symbolp pvar-names) ()
	  "The arguments to *deallocate-*defvars must be the NAMES of pvars."
	  )

  (cond
    (pvar-names
     ;; handle the pvars specified by the user
     (dolist (pvar-name pvar-names)
       (let ((pvar-info (find pvar-name *all-*defvar-specifications* :test #'eq :key #'*defvar-specification-name)))
	 (cond
	   ((null pvar-info)
	    (warn "The symbol ~S, having value ~S, is not currently defined as a *DEFVAR."
		  pvar-name
		  (if (boundp pvar-name) (symbol-value pvar-name) "#<UNBOUND>")
		  )
	    (cond
	      ((y-or-n-p "Make it unbound anyway and continue")
	       (makunbound pvar-name)
	       )
	      ((y-or-n-p "Ignore it and continue"))
	      ))
	   (t (delete-old-*defvar-definition pvar-name))
	   ))))

    ;; do this only if there are some *defvars around to delete

    (*all-*defvar-specifications*
     ;; user didn't specify any pvars, go through all of them, prompting for each.
     (let ((list ()) (*print-pretty* nil))
       (format *query-io* "~%Deallocate which pvars?~%~20A ~25A ~25A" 
	       "Pvar Name" "Initial Value" "Declaration")
       (dolist (defvar *all-*defvar-specifications*)
	 (let* ((pvar-name (*defvar-specification-name defvar))
		(pvar-initial-value (*defvar-specification-initial-value-form defvar))
		(pvar-declaration (get pvar-name 'type))
		)
	   (when (y-or-n-p
		 "~(~20S ~25S ~25A~)"
		 pvar-name (or pvar-initial-value 'none) (or pvar-declaration '(pvar *))
		 )
	     (push pvar-name list)
	     )))
       (if list (apply #'*deallocate-*defvars list))
       ))

    )

  nil

  )

(defun deallocate-pvars-for-vp-set

       (vp-set
	&key
	(delete-*defvars t)
	(deallocate-*defvar-memory t)
	(delete-allocate!!-pvars t)
	)
  
  (when delete-allocate!!-pvars
    (mapc
      #'(lambda (pvar)
	  (when (eq (pvar-vp-set pvar) vp-set) (*deallocate-internal pvar :allocate!!))
	  )
      *all-allocate!!-pvars*
      ))

  (when deallocate-*defvar-memory
    (mapc
      #'(lambda (*defvar-specification)
	  (when (eq (*defvar-specification-vp-set-name *defvar-specification) (vp-set-name vp-set))
	    (when (and (boundp (*defvar-specification-name *defvar-specification))
		       (pvarp (eval (*defvar-specification-name *defvar-specification)))
		       )
	      (*deallocate-internal (eval (*defvar-specification-name *defvar-specification)) :*defvar)
	      (makunbound (*defvar-specification-name *defvar-specification))
	      )))
      *all-*defvar-specifications*
      ))

  (when delete-*defvars
    (mapc
      #'(lambda (*defvar-specification)
	  (when (eq (*defvar-specification-vp-set-name *defvar-specification) (vp-set-name vp-set))
	    (setq *all-*defvar-specifications*
		  (delete *defvar-specification *all-*defvar-specifications* :test #'eq)
		  )
	    (when (*defvar-specification-name *defvar-specification)
	      (makunbound (*defvar-specification-name *defvar-specification))
	      )))
      *all-*defvar-specifications*
      ))

  )

(defun delete-old-*defvar-definition (name)
  (let ((specification (find name *all-*defvar-specifications* :key #'*defvar-specification-name)))
    (if (and specification (*defvar-specification-initial-value-function specification))
	(fmakunbound (*defvar-specification-initial-value-function specification))))
  (setq *all-*defvar-specifications*
	(delete name *all-*defvar-specifications* :test #'eq :key #'*defvar-specification-name))
  (*deallocate-*defvar-if-possible name)
  (makunbound name))


(defun *deallocate-*defvar-if-possible (variable)
  (when (and (*lisp-runnable-p)
	     (boundp variable)
	     (pvarp (symbol-value variable))
	     (integerp (pvar-location (symbol-value variable)))
	     (plusp (pvar-location (symbol-value variable)))
	     )
    (*deallocate-internal (symbol-value variable) :*defvar)
    ))



(defun allocate-*defvars-for-vp-set (vp-set)
  (let ((vp-set-name (vp-set-name vp-set)))
    (mapc
      #'(lambda (*defvar-specification)
	  (when (eq vp-set-name (*defvar-specification-vp-set-name *defvar-specification))
	    (let ((in-vp-set-definition-p (*defvar-specification-in-vp-set-definition-p *defvar-specification)))
	      (*defvar-1
		(*defvar-specification-name *defvar-specification)
		(*defvar-specification-initial-value-form *defvar-specification)
		vp-set-name
		)
	      (setf (*defvar-specification-in-vp-set-definition-p
		      (find (*defvar-specification-name *defvar-specification) *all-*defvar-specifications*
			    :test #'eq :key #'*defvar-specification-name
			    ))
		    in-vp-set-definition-p
		    ))))
      *all-*defvar-specifications*
      )))

