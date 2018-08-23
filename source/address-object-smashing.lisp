;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Muser: yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(*proclaim '(ftype (function () (pvar boolean)) css!!))

(defun css!! ()
  (*let (result)
    (declare (type (pvar boolean) result))
    (declare (return-pvar-p t))
    (*all (*set result nil!!))
    (*set result t!!)
    result
    ))

(defmacro illegal-geometry-p!! (ao)
  `(=!! (!! (the fixnum *illegal-geometry-id*)) (address-object-geometry-id!! (the (pvar address-object) ,ao)))
  )

;;; An address object is legal iff every active processor
;;; contains a valid geometry id.

(defun legal-address-object-p (ao)
  (*and (/=!! (!! (the fixnum *illegal-geometry-id*)) (address-object-geometry-id!! (the (pvar address-object) ao))))
  )

(defun check-for-legal-address-object (ao function)
  (when (not (legal-address-object-p ao))
    (format t "~S: Possible user or internal error.  Address object has invalid geometry id in active processor" function)
    ;; (ppp-address-object ao)
    (error "Invalid address object")
    ))


;;; An address object is cacheable if, a) it is legal, and b)
;;; every processor which contain a valid address object , independent
;;; of the currently selected set,
;;; contains an address object with the same geometry id.

;;; This function returns either NIL or the geometry id that the
;;; address object is cacheable to.

(defun cacheable-address-object-p (ao function &key (check-legality t))
  (when check-legality (check-for-legal-address-object ao function))
  (*all
    (declare (return-pvar-p nil))
    (*when (/=!! (!! (the fixnum *illegal-geometry-id*)) (address-object-geometry-id!! (the (pvar address-object) ao)))
      (declare (return-pvar-p nil))
      (let (min max)
	(when (*or t!!)
	  (setq min (*min (address-object-geometry-id!! (the (pvar address-object) ao))))
	  (setq max (*max (address-object-geometry-id!! (the (pvar address-object) ao))))
	  (and (eql min max) min)
	  )))))


(defun cache-address-object-pvar-if-possible (ao function &key (check-legality t))
  (let ((cacheable-geometry-id (cacheable-address-object-p ao function :check-legality check-legality)))
    (if cacheable-geometry-id
	(cache-address-object-pvar ao cacheable-geometry-id)
	(decache-address-object-pvar ao)
	)))



(defun cached-address-object-p (ao)
  (address-object-cached-geometry-id ao)
  )


;;; This function possibly side effects address-object-pvar by coercing into
;;; an address-object-pvar with a different geometry id in the active
;;; processors.  This function assumes that all the active processors
;;; contain address objects with the same, legal, geometry id.  If the
;;; address object is coerced, then it is decached.

;;; If the coercion is not possible, then, if error is NIL, NIL is
;;; returned, otherwise an error is signalled.  If the coercion
;;; is possible, it is done, and T is returned.


(defun-wcefi smash-address-object-with-single-geometry-to-new-geometry

       (new-geometry old-geometry address-object-pvar &key (error nil))

  ;; We have a single geometry.  Is it the same as the
  ;; geometry id of the vp set we are supposed to be coercing to?
  ;; If so our work is already done.

  (if (eq old-geometry new-geometry)

      t

      ;; First make sure that the ranks of the two geometries
      ;; are equal.  If not, it is impossible to coerce the
      ;; address object into the new geometry!

      (if (not (eql (geometry-rank old-geometry) (geometry-rank new-geometry)))

	  (if error
	      (error "You are trying to use a ~D dimensional address object, ~S, to reference into a vp set~@
                      that has ~D dimensions.  This cannot work."
		     (geometry-rank old-geometry) address-object-pvar (geometry-rank new-geometry)
		     )
	      nil
	      )

	  (*let (cube-address-pvar temp-grid-coordinates-array)
	    (declare (type max-cube-address-pvar-type cube-address-pvar))
	    (declare (type (grid-address-pvar-array-type (geometry-rank old-geometry)) temp-grid-coordinates-array))
	    (declare (return-pvar-p nil))
	    nil

	    (*set cube-address-pvar (address-object-cube-address!! (the (pvar address-object) address-object-pvar)))

	    ;; Figure out the grid addresses given the cube address and old geometry.

	    (store-pvar-grid-addresses-from-pvar-cube-address-into-array
	      cube-address-pvar old-geometry temp-grid-coordinates-array
	      )

	    ;; See whether the grid addresses are valid in the new geometry.

	      (if (not (pvar-array-of-grid-coordinates-fit-into-geometry-p new-geometry temp-grid-coordinates-array))

		  (if error
		      (error "You are trying to use the address object pvar ~S to reference grid points in~@
                              a vp set which has dimensions ~S, but the address object pvar contains addresses~@
                              which index outside those dimensions."
			     address-object-pvar (geometry-dimensions new-geometry)
			     )
		      nil
		      )

		  ;; If so, smash the geometry id and convert the grid addresses back
		  ;; into a cube address, this time inside the new geometry.

		  (progn
		    (*setf (address-object-geometry-id!! (the (pvar address-object) address-object-pvar))
			   (!! (the fixnum (geometry-id new-geometry)))
			   )
		    (*setf (address-object-cube-address!! (the (pvar address-object) address-object-pvar))
			   (translate-pvar-array-of-grid-coordinates-into-pvar-cube-address!!
			     temp-grid-coordinates-array new-geometry nil
			     ))
		    (decache-address-object-pvar address-object-pvar)
		    t
		    )

		  )))))


;;; This function guarentees to coerce the elements of address-object-pvar in the active
;;; processors into the geometry of the new vp set, or signal an error
;;; if this is impossible.

;;; It may or may not coerce the elements of address-object-pvar in the
;;; non-active processors, and it may or may not leave address-object-pvar
;;; in a cached state.

;;; smash-address-object-to-new-vp-set!! is executed for side effect.  It
;;; returns address-object-pvar.


(defun-wcefi smash-address-object-to-new-vp-set!! (address-object-pvar new-vp-set)

  ;; If there are no processors active there is nothing that has to be done.

  (when (not (*or t!!))
    (return-from smash-address-object-to-new-vp-set!! address-object-pvar)
    )

  (safety-check
    (new-pvar-check address-object-pvar 'smash-address-object-to-new-vp-set!!)
    (assert (and (structure-pvar-p address-object-pvar) (eq 'address-object (structure-pvar-name address-object-pvar)))
	    ()
	    "Error.  The object, ~S, is not an address object pvar."
	    address-object-pvar
	    )
    (check-for-legal-address-object address-object-pvar 'smash-address-object-to-new-vp-set!!)
    )

  (let ((new-geometry (vp-set-geometry new-vp-set))
	(old-geometry-id (cached-address-object-p address-object-pvar))
	)

    (when old-geometry-id

      (let ((old-geometry (geometry-from-id old-geometry-id)))

	;; This is an address object pvar which has a cached geometry.

	(safety-check
	  (when (not (cacheable-address-object-p address-object-pvar 'smash-address-object-to-new-vp-set!!))
	    (error "Internal error.  The address-object ~S is cached, but it should not be cached." address-object-pvar)
	    ))

	;; If the cached geometry is the same as the geometry we wish to
	;; coerce to, there is nothing to do.

	(when (eq new-geometry (cached-address-object-p address-object-pvar))
	  (return-from smash-address-object-to-new-vp-set!! address-object-pvar)
	  )

	;; If it's cached, then all the legal geometry ids are
	;; the same.  But the pointers stored in the non-active
	;; processors may or may be coercible to the new
	;; geometry.  If they are not, we can't error out, and
	;; we cannot cache the geometry.  If they are, we can
	;; cache the new geomety.

	(*let ((css (css!!)))
	  (declare (type boolean-pvar css))
	  (declare (return-pvar-p nil))

	  ;; Coerce all the address objects in the active set.
	  ;; Error out if any cannot be coerced. 

	  (let ((cache? t))

	    (smash-address-object-with-single-geometry-to-new-geometry
	      new-geometry old-geometry address-object-pvar :error t
	      )

	    ;; Select those processors not in the active set which
	    ;; have valid geometries.  If there aren't any, there is
	    ;; nothing to do.  If any cannot be coerced then we
	    ;; cannot cache the address object pvar.

	    (*all
	      (declare (return-pvar-p nil))
	      (*when (and!! (not!! css) (not!! (illegal-geometry-p!! address-object-pvar)))
		(declare (return-pvar-p nil))
		(when (*or t!!)
		  (setq cache? (smash-address-object-with-single-geometry-to-new-geometry
				 new-geometry old-geometry address-object-pvar :error nil
				 )))))

	    ;; Recache the address object pvar if possible.

	    (when cache? (cache-address-object-pvar address-object-pvar (geometry-id new-geometry)))

	    ))

	(return-from smash-address-object-to-new-vp-set!! address-object-pvar)

	))

    ;; The geometry is not cached.  Let's see if it is cacheable.

    (let ((old-cached-geometry
	    (cache-address-object-pvar-if-possible address-object-pvar 'smash-address-object-to-new-vp-set!!)
	    ))

      ;; If it is, recurse, and handle that case in the code above.

      (when old-cached-geometry
	(smash-address-object-to-new-vp-set!! address-object-pvar new-vp-set)
	(return-from smash-address-object-to-new-vp-set!! address-object-pvar)
	)

      ;; It's not cacheable.  We will first map over all the geometries
      ;; present in the active set, coercing the address objects of
      ;; each geometry to the new geometry.

      (let ((cache? t))

	(*map-geometries
	  #'(lambda (old-geometry-id)
	      (smash-address-object-with-single-geometry-to-new-geometry
		new-geometry (geometry-from-id old-geometry-id) address-object-pvar :error t
		))
	  address-object-pvar
	  )

	;; Now we will map over all the geometries not in the active set.
	;; If all the address objects in each of this geometries is
	;; coercible, then we can cache

	(*let ((css (css!!)))
	  (declare (type (pvar boolean) css))
	  (declare (return-pvar-p nil))
	  (*all
	    (declare (return-pvar-p nil))
	    (*when (and!! (not!! css) (not!! (illegal-geometry-p!! address-object-pvar)))
	      (declare (return-pvar-p nil))
	      (when (*or t!!)
		(*map-geometries
		  #'(lambda (geometry-id)
		      (setq cache?
			    (and cache?
				 (smash-address-object-with-single-geometry-to-new-geometry
				   new-geometry (geometry-from-id geometry-id) address-object-pvar :error nil
				   ))))
		  address-object-pvar
		  )))))

	(when cache? (cache-address-object-pvar address-object-pvar (geometry-id new-geometry)))

	))

    address-object-pvar

    ))




(defun smash-front-end-address-object-with-single-geometry-to-new-vp-set
       
       (new-geometry old-geometry address-object)
  
  ;; Are the two geometries the same.  If so there is nothing to do.

  (when (eq old-geometry new-geometry)
    (return-from smash-front-end-address-object-with-single-geometry-to-new-vp-set nil)
    )
  
  ;; First make sure that the ranks of the two geometries
  ;; are equal.  If not, it is impossible to coerce the
  ;; address object into the new geometry!
      
  (if (not (eql (geometry-rank old-geometry) (geometry-rank new-geometry)))
	  
      (error "You are trying to access a processor in a vp set with ~D dimensions, using~@
              a front-end address object, ~S, that has ~D dimensions.  This is impossible."
	     (geometry-rank new-geometry) address-object (geometry-rank old-geometry)
	     )
	  
      (let (cube-address temp-grid-coordinates-array)
	(setq cube-address (address-object-cube-address address-object))
	(setq temp-grid-coordinates-array (make-array (geometry-rank old-geometry)))
	(store-grid-addresses-from-cube-address-into-array cube-address old-geometry temp-grid-coordinates-array)

	(if (not (array-of-grid-coordinates-fit-into-geometry-p new-geometry temp-grid-coordinates-array))

	    (error "The front end address object ~S, which references a processor at~@
                    cube address ~D, is being used to reference into a vp set of~@
                    dimensions ~S, but that vp set is too small to have a cube address ~D"
		   address-object
		   (address-object-cube-address address-object)
		   (geometry-dimensions new-geometry)
		   (address-object-cube-address address-object)
		   )
	      
	    (progn
	      (setf (address-object-geometry-id address-object) (geometry-id new-geometry))
	      (setf (address-object-cube-address address-object)
		    (translate-array-of-grid-coordinates-into-cube-address temp-grid-coordinates-array new-geometry nil)
		    ))
		  
	    )))

  nil

  )


(defun smash-address-object-to-new-vp-set (ao vp-set)
  (if (not (eql (address-object-geometry-id ao) (vp-set-geometry-id vp-set)))
      (smash-front-end-address-object-with-single-geometry-to-new-vp-set
	(vp-set-geometry vp-set) (geometry-from-id (address-object-geometry-id ao)) ao
	))
  ao
  )




(defun verify-ao-constant-grid-addresses (ao tag &rest constant-grid-addresses)
  (dotimes (j (length constant-grid-addresses))
    (assert (*and (=!! (address-nth!! ao (!! j)) (!! (nth j constant-grid-addresses)))) () "~A" tag)
    ))

(defun test-address-object-smashing ()
  (let-vp-set (x1 (create-vp-set '(2 2)))
    (let-vp-set (x2 (create-vp-set '(4 2)))
      (let-vp-set (x3 (create-vp-set '(4 4)))
	(let-vp-set (y1 (create-vp-set '(2 2 2)))

	  (*with-vp-set x1

	    (*let (ao1 (ao2 (make-address-object!!)) (ao3 (grid!! (!! 0) (!! 0))) (ao4 (grid-relative!! (!! 0) (!! 0))))
	      (declare (type (pvar address-object) ao1 ao2 ao3 ao4))

	      (format t "~%Testing non-caching of non-initialized address objects")
	      (assert (not (cached-address-object-p ao1)))
	      (assert (not (cached-address-object-p ao2)))

	      (format t "~%Testing caching of initialized address objects using grid!! and grid-relative!!")
	      (assert (eql (cached-address-object-p ao3) (vp-set-geometry-id x1)))
	      (assert (eql (cached-address-object-p ao4) (vp-set-geometry-id x1)))

	      (format t "~%Testing simple cached coercion to larger geometry")
	      (smash-address-object-to-new-vp-set!! ao3 x2)
	      (assert (eql (cached-address-object-p ao3) (vp-set-geometry-id x2)))
	      (smash-address-object-to-new-vp-set!! ao3 x3)
	      (assert (eql (cached-address-object-p ao3) (vp-set-geometry-id x3)))

	      (format t "~%Testing cached coercion back to smaller geometry")
	      (smash-address-object-to-new-vp-set!! ao3 x1)
	      (assert (eql (cached-address-object-p ao3) (vp-set-geometry-id x1)))

	      (verify-ao-constant-grid-addresses ao3 0 0)

	      (format t "~%Testing non-cached coercion to larger geometry")
	      (decache-address-object-pvar ao3)
	      (smash-address-object-to-new-vp-set!! ao3 x1)
	      (assert (eql (cached-address-object-p ao3) (vp-set-geometry-id x1)))
	      (decache-address-object-pvar ao3)
	      (smash-address-object-to-new-vp-set!! ao3 x2)
	      (assert (eql (cached-address-object-p ao3) (vp-set-geometry-id x2)))
	      (format t "~%Testing non-cached coercion back to smaller geometry")
	      (decache-address-object-pvar ao3)
	      (smash-address-object-to-new-vp-set!! ao3 x1)
	      (assert (eql (cached-address-object-p ao3) (vp-set-geometry-id x1)))

	      (verify-ao-constant-grid-addresses ao3 0 0)

	      ))

	  (*with-vp-set x1

	    (format t "~%Testing coercion of ao with subselection with *illegal-geometry-id* some places into other geometry")

	    (*let (ao1)
	      (declare (type (pvar address-object) ao1))
	      (*setf (address-object-cube-address!! ao1) (self-address!!))
	      (*setf (address-object-geometry-id!! ao1)
		     (if!! (evenp!! (self-address!!))
			   (!! (vp-set-geometry-id x2))
			   (!! *illegal-geometry-id*)
			   ))
	      (*when (evenp!! (self-address!!))
		(smash-address-object-to-new-vp-set!! ao1 x1)
		)
	      (assert (*and (=!! (self-address!!) (address-object-cube-address!! ao1))))
	      (*when (evenp!! (self-address!!))
		(assert (*and (=!! (!! (vp-set-geometry-id x1)) (address-object-geometry-id!! ao1))))
		)
	      (assert (eql (cached-address-object-p ao1) (vp-set-geometry-id x1)))
	      )

	    (format t "~%Testing coercion of ao with two different geometries into third geometry")

	    (*let (ao1)
	      (declare (type (pvar address-object) ao1))
	      (*setf (address-object-cube-address!! ao1) (self-address!!))
	      (*setf (address-object-geometry-id!! ao1)
		     (if!! (evenp!! (self-address!!))
			   (!! (vp-set-geometry-id x2))
			   (!! (vp-set-geometry-id x3))
			   ))
	      (smash-address-object-to-new-vp-set!! ao1 x1)
	      (assert (*and (=!! (self-address!!) (address-object-cube-address!! ao1))))
	      (assert (*and (=!! (!! (vp-set-geometry-id x1)) (address-object-geometry-id!! ao1))))
	      (assert (eql (cached-address-object-p ao1) (vp-set-geometry-id x1)))
	      )

	    (format t "~%Testing coercion of ao with two different geometries into third geometry with subselection")

	    (*let (ao1)
	      (declare (type (pvar address-object) ao1))
	      (*setf (address-object-cube-address!! ao1) (self-address!!))
	      (*setf (address-object-geometry-id!! ao1)
		     (if!! (evenp!! (self-address!!))
			   (!! (vp-set-geometry-id x2))
			   (!! (vp-set-geometry-id x3))
			   ))
	      (*when (evenp!! (self-address!!))
		(smash-address-object-to-new-vp-set!! ao1 x1)
		)
	      (assert (*and (=!! (self-address!!) (address-object-cube-address!! ao1))))
	      (assert (*and (=!! (!! (vp-set-geometry-id x1)) (address-object-geometry-id!! ao1))))
	      (assert (eql (cached-address-object-p ao1) (vp-set-geometry-id x1)))
	      )

	    (format t "~%Coercion of ao with two geometries into third geometry with subselection where one cannot be coerced")

	    (*let (ao1)
	      (declare (type (pvar address-object) ao1))
	      (*setf (address-object-cube-address!! ao1) (self-address!!))
	      (*setf (address-object-geometry-id!! ao1)
		     (if!! (evenp!! (self-address!!))
			   (!! (vp-set-geometry-id x2))
			   (!! (vp-set-geometry-id y1))
			   ))
	      (*when (evenp!! (self-address!!))
		(smash-address-object-to-new-vp-set!! ao1 x1)
		)
	      (assert (*and (=!! (self-address!!) (address-object-cube-address!! ao1))))
	      (*when (evenp!! (self-address!!))
		(assert (*and (=!! (!! (vp-set-geometry-id x1)) (address-object-geometry-id!! ao1))))
		)
	      (*when (oddp!! (self-address!!))
		(assert (*and (=!! (!! (vp-set-geometry-id y1)) (address-object-geometry-id!! ao1))))
		)
	      (assert (eql (cached-address-object-p ao1) nil))
	      )

	    ))))))
