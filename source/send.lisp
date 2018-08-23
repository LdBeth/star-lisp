;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-I; MUSER: YES-*-

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


(defun no-collisions-combiner (x y)
  (declare (ignore x y))
  (error "*PSET: A processor received more than one message with :NO-COLLISIONS specified.")
  )

(defun overwrite-combiner (x y) (declare (ignore x)) y)

(defun default-combiner (x y)
  (declare (ignore x y))
  (error "*PSET: A processor received more than one message.")
  )

(defun and-combiner (x y) (and x y))
(defun or-combiner (x y) (or x y))
(defun logand-combiner (x y) (logand x y))
(defun logior-combiner (x y) (logior x y))
(defun logxor-combiner (x y) (logxor x y))
(defun max-combiner (x y) (max x y))
(defun min-combiner (x y) (min x y))
(defun add-combiner (x y) (+ x y))

(defun queue-combiner (x y)
  (declare (ignore x y))
  (error "This should be being handled specially")
  )

(eval-when (:load-toplevel :execute)
  (mapc
    #'(lambda (symbol property-value)
	(setf (get symbol '*pset-function) property-value)
	)
    '(:NO-COLLISIONS
      :OVERWRITE
      :DEFAULT
      :AND AND!! AND
      :OR OR!! OR
      :LOGAND LOGAND!! LOGAND
      :LOGIOR LOGIOR!! LOGIOR
      :LOGXOR LOGXOR!! LOGXOR
      :MAX MAX!! MAX
      :MIN MIN!! MIN
      :ADD ADD!! +
      :QUEUE
      )
    `(
      no-collisions-combiner
      overwrite-combiner
      default-combiner
      and-combiner and-combiner and-combiner
      or-combiner or-combiner or-combiner
      logand-combiner logand-combiner logand-combiner
      logior-combiner logior-combiner logior-combiner
      logxor-combiner logxor-combiner logxor-combiner
      max-combiner max-combiner max-combiner
      min-combiner min-combiner min-combiner
      add-combiner add-combiner add-combiner
      queue-combiner
      )))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro *pset
#+:CCL	    (&environment env
	     combiner source-pvar dest-pvar send-address-pvar
	     &key
	     (notify nil)
	     (vp-set nil)
	     (collision-mode nil)
	     (combine-with-dest nil)
	     )
#-:CCL	    (combiner source-pvar dest-pvar send-address-pvar
	     &key
	     (notify nil)
	     (vp-set nil)
	     (collision-mode nil)
	     (combine-with-dest nil)
	     &environment env
	     )
    (when collision-mode (warn "Using a collision mode argument in *PSET under Release 5.0 *Lisp is obsolete"))
    `(*pset-internal
       ,combiner ,source-pvar ,(recursive-alias!! dest-pvar env) ,send-address-pvar ,notify ,vp-set ,combine-with-dest
       )))


(defun check-for-invalid-send-addresses (send-address-pvar vp-set)
  (let ((vp-set-size (vp-set-size vp-set)))
    (declare (type fixnum vp-set-size))
    (with-selected-general-pvar-arrays
      (processor) (send-array) (send-address-pvar)
      (let ((address (aref send-array processor)))
	(when (not (and (integerp address) (< -1 address vp-set-size)))
	  (error "Invalid send address, ~S, at processor ~D, for destination Vp Set ~S, while executing a *PSET"
		 address processor vp-set
		 ))))))


(defun incompatible-types-for-communication (pvar1 pvar2)
  (or
    (and (simple-general-pvar-p pvar1) (non-scalar-pvar-p pvar2))
    (and (non-scalar-pvar-p pvar1) (simple-general-pvar-p pvar2))
    (and (array-pvar-p pvar1) (structure-pvar-p pvar2))
    (and (structure-pvar-p pvar1) (array-pvar-p pvar2))
    ))



(defun check-*pset-arguments (dest-pvar source-pvar address-pvar notify-pvar combiner vp-set combine-with-dest)
  (new-pvar-check-lvalue-no-vp-check dest-pvar '*pset)
  (when notify-pvar (new-pvar-check-lvalue-no-vp-check notify-pvar '*pset))
  (new-two-pvar-check source-pvar address-pvar '*pset)
  (assert (or (null vp-set) (eq vp-set (pvar-vp-set dest-pvar))) ()
	  "You specified Vp Set ~S as a keyword argument to *PSET, but the destination pvar belongs to Vp Set ~S"
	  vp-set (pvar-vp-set dest-pvar)
	  )
  (assert (not (eq dest-pvar notify-pvar)) () "You cannot specify the destination and notify pvars as identical")
  (assert (or (null notify-pvar) (eq (pvar-vp-set dest-pvar) (pvar-vp-set notify-pvar))) ()
	  "For *PSET, the destination pvar and the notify pvar must be in the same vp set"
	  )
  (and notify-pvar (assert (general-pvar-p notify-pvar) () "The notify pvar cannot be an array or structure pvar"))
  (when (not (and (eq :structure (pvar-type address-pvar)) (eq 'address-object (pvar-structure-name address-pvar))))
    (check-for-invalid-send-addresses address-pvar (pvar-vp-set dest-pvar))
    )
  (if (keywordp combiner)
      (assert (get combiner '*pset-function) () "Unknown combiner keyword" combiner)
      (assert (or (and (symbolp combiner) (fboundp combiner))
		  (functionp combiner)
		  )
	      ()
	      "Combiner ~S is not callable as a function"
	      combiner
	      ))
  (when (eq combiner :queue)
    (assert (eq :array (pvar-type dest-pvar)) () "The destination pvar using :QUEUE must be an array pvar")
    (assert (eql 1 (*array-rank dest-pvar)) () "The destination pvar using :QUEUE must be a 1-d array pvar") 
    (assert (null combine-with-dest) () "Using :QUEUE, specifying :COMBINE-WITH-DEST makes no sense")
    ))
  


(defun *pset-internal (combiner source-pvar dest-pvar address-pvar notify-pvar vp-set combine-with-dest)

  nil

  (simple-pvar-argument!! source-pvar address-pvar)

  (safety-check
    (check-*pset-arguments dest-pvar source-pvar address-pvar notify-pvar combiner vp-set combine-with-dest)
    )

  (*let ((cube-address-pvar
	   (if (structure-pvar-p address-pvar)
	       (address-object-cube-address!! (smash-address-object-to-new-vp-set!! address-pvar (pvar-vp-set dest-pvar)))
	       address-pvar
	       )))


    nil

    (let

      ((dest-copied? nil)
       (notify-copied? nil)
       (old-dest-pvar dest-pvar)
       (old-notify-pvar notify-pvar)
       (combining-function (or (get combiner '*PSET-FUNCTION) combiner))
       (dest-vp-set (pvar-vp-set dest-pvar))
       (source-vp-set *current-vp-set*)
       )

      (*pset-internal-1
	combiner source-pvar dest-pvar address-pvar notify-pvar vp-set
	cube-address-pvar dest-copied? notify-copied? old-dest-pvar
	old-notify-pvar combining-function dest-vp-set source-vp-set
	combine-with-dest
	))))


(defun *pset-internal-1

       (
	combiner source-pvar dest-pvar address-pvar notify-pvar vp-set
	cube-address-pvar dest-copied? notify-copied? old-dest-pvar
	old-notify-pvar combining-function dest-vp-set source-vp-set
	combine-with-dest
	)
	
      (let*
	((dest-pvar
	   (if (or (eq dest-pvar source-pvar) (eq dest-pvar cube-address-pvar))
	       (*with-vp-set (pvar-vp-set dest-pvar)
		 (let ((result (allocate-similar-temp-pvar dest-pvar)))
		   nil
		   (setf (pvar-lvalue? result) t)
		   (setf (pvar-name result) 'DEST-PVAR-TEMP)
		   (setq dest-copied? t)
		   (*set result dest-pvar)
		   result
		   ))
	       dest-pvar
	       ))
	 (notify-pvar
	   (if notify-pvar
	       (if (or (eq notify-pvar source-pvar) (eq notify-pvar cube-address-pvar ))
		   (*with-vp-set (pvar-vp-set dest-pvar)
		     (let ((result (allocate-temp-general-pvar)))
		       nil
		       (setf (pvar-lvalue? result) t)
		       (set (pvar-name result) 'NOTIFY-PVAR-TEMP)
		       (setq notify-copied? t)
		       result
		       ))
		   notify-pvar
		   )
	       (*with-vp-set (pvar-vp-set dest-pvar)
		 (let ((result (allocate-temp-general-pvar)))
		   nil
		   (setf (pvar-lvalue? result) t)
		   (setf (pvar-name result) 'NOTIFY-PVAR-TEMP)
		   (setf (pvar-vp-set result) (pvar-vp-set dest-pvar))
		   result
		   ))))
	 )


	(*pset-internal-2
	  combiner source-pvar dest-pvar address-pvar notify-pvar vp-set
	  cube-address-pvar dest-copied? notify-copied? old-dest-pvar
	  old-notify-pvar combining-function dest-vp-set source-vp-set
	  combine-with-dest
	  )))


(defun *pset-internal-2

       (
	combiner source-pvar dest-pvar address-pvar notify-pvar vp-set
	cube-address-pvar dest-copied? notify-copied? old-dest-pvar
	old-notify-pvar combining-function dest-vp-set source-vp-set
	combine-with-dest
	)

  (declare (ignore address-pvar vp-set))

  (*with-vp-set dest-vp-set

    nil
    (*let (received-message? received-message-yet?)
      nil
      (*all nil (*set received-message? nil!!) (*set received-message-yet? nil!!))

      (*with-vp-set source-vp-set

        nil
	(with-selected-general-pvar-arrays
	  (j) (received-array address-array) (received-message? cube-address-pvar )
	  nil
	  (setf (aref received-array (aref address-array j)) t)
	  )

	(*with-vp-set dest-vp-set nil (*all nil (*when received-message? nil (*set notify-pvar t!!))))

	(if (eq combiner :queue)
	    (*send-with-queue dest-pvar source-pvar cube-address-pvar)
	    (*send
	      dest-pvar source-pvar cube-address-pvar received-message-yet?
	      combiner combining-function combine-with-dest
	      ))

	(when dest-copied? (*all nil (*when received-message? nil (*set old-dest-pvar dest-pvar))))
	(when notify-copied? (*all nil (*when received-message? nil (*set old-notify-pvar notify-pvar))))

	)))

  (values)

  )


(defun *send

    (dest-pvar source-pvar address-pvar received-message-yet? combiner combining-function combine-with-dest)

  ;; received-message-yet? contains NIL everywhere initially.

  (cond
    
    ;; pvars containing only scalar quantities.
    ;; Just do the send.
    
    ((and (scalar-pvar-p source-pvar) (scalar-pvar-p dest-pvar))
     (let ((any-active nil))
       (with-selected-general-pvar-arrays
	   (j)
	 (dest-array source-array address-array received-message-yet-array)
	 (dest-pvar source-pvar address-pvar received-message-yet?)
	 (setq any-active t)
	 (let ((destination-address (aref address-array j)))
	   (if (aref received-message-yet-array destination-address)
	       (setf (aref dest-array destination-address)
		     (funcall combining-function (aref dest-array destination-address) (aref source-array j)))
	       (progn
		 (if combine-with-dest
		     (setf (aref dest-array destination-address)
			   (funcall combining-function
				    (aref dest-array destination-address) (aref source-array j)))
		     (setf (aref dest-array destination-address) (aref source-array j))
		     )
		 (setf (aref received-message-yet-array destination-address) t)
		 ))))
       (when any-active (make-non-void dest-pvar))
       ))
    
    ;; Two arrays.

    ((and (array-pvar-p source-pvar) (array-pvar-p dest-pvar))
     (when (not (member combiner '(:NO-COLLISIONS :DEFAULT :OVERWRITE)))
       (error "At least currently, the combiner ~S makes no sense when applied to array pvars" combiner)
       )
     (let ((lisp-array-holding-source-pvars (pvar-array source-pvar))
	   (lisp-array-holding-dest-pvars (pvar-array dest-pvar))
	   )
       (with-many-array-elements-iterated
	 (dest-element source-element)
	 (lisp-array-holding-dest-pvars lisp-array-holding-source-pvars)
	 (*send
	   dest-element source-element address-pvar received-message-yet?
	   combiner combining-function combine-with-dest
	   )
	 (*with-vp-set (pvar-vp-set received-message-yet?) (*all (*set received-message-yet? nil!!)))
	 )))
    
    ;; Two structures.

    ((and (structure-pvar-p source-pvar) (structure-pvar-p dest-pvar))
     (when (not (member combiner '(:NO-COLLISIONS :DEFAULT :OVERWRITE)))
       (error "At least currently, the combiner ~S makes no sense when applied to structure pvars" combiner)
       )
     (let ((lisp-structure-holding-source-pvars (pvar-structure source-pvar))
	   (lisp-structure-holding-dest-pvars (pvar-structure dest-pvar))
	   )
       (with-structure-elements-iterated
	 (
	  (dest-slot-pvar source-slot-pvar)
	  (lisp-structure-holding-dest-pvars lisp-structure-holding-source-pvars)
	  (structure-pvar-type-front-end-slot-accessors
	    (pvar-canonical-pvar-type source-pvar)
	    ))
	 (*send
	   dest-slot-pvar source-slot-pvar address-pvar received-message-yet?
	   combiner combining-function combine-with-dest
	   )
	 (*with-vp-set (pvar-vp-set received-message-yet?) (*all (*set received-message-yet? nil!!)))
	 )))
    
    ((incompatible-types-for-communication source-pvar dest-pvar)
     (error "You cannot *PSET a pvar of type ~S into a pvar of type ~S"
	    (pvar-canonical-pvar-type source-pvar) (pvar-canonical-pvar-type dest-pvar)
	    ))
    
    ;; General pvars possibly containing arrays and structures.
    ;; Do it THE SLOW WAY, one processor at a time using (*SETF (PREF ...
    ;; If anyone ever really tries to do this they deserve all the time and consing
    ;; it takes!
    
    (t
     (error "Internal error.  You should not have gotten here!")
     )
    
    ))



