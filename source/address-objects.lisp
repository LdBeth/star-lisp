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


;;;; This file basically implements GRID!!, and its
;;;; attendant functions such as ADDRESS-NTH!! and
;;;; ADDRESS-PLUS-NTH!!.  It does not actually have
;;;; the definitions of PREF!! and friends.
 
(defun self!! ()
  (let ((result (make-address-object!!
		  :geometry-id (!! (the fixnum (vp-set-geometry-id *current-vp-set*)))
		  :cube-address (self-address!!)
		  )))
    (setf (pvar-address-object-geometry-id result) (vp-set-geometry-id *current-vp-set*))
    (initialize-address-object-in-non-active-processors result)
    result
    ))



(*proclaim '(ftype (function (&rest t) (pvar address-object)) grid!! grid-relative!!))
(*proclaim '(ftype (function (t t t &rest t) (pvar address-object)) address-plus-nth!!))
(*proclaim '(ftype (function (t t) max-grid-address-pvar-type) address-nth!!))
(*proclaim '(ftype (function (t) geometry-rank-pvar-type) address-rank!!))

(*proclaim '(ftype (function (t t t &optional t) max-cube-address-pvar-type)
		   translate-list-of-pvar-grid-coordinates-into-pvar-cube-address!!
		   translate-pvar-array-of-grid-coordinates-into-pvar-cube-address!!
		   ))

(*proclaim '(ftype (function (t t t) max-grid-address-pvar-type)
		   grid-address-pvar-from-cube-address-pvar!!
		   ))


;;;; From here on follows the stuff needed to implement GRID, GRID!!
;;;; GRID-RELATIVE!!.


(defun grid (&rest integers)

  "The front end version of GRID!!.  Returns an
   address-object DEFSTRUCT.
  "

  (when (null integers) (error "You cannot have zero addresses.  Sorry."))
  (assert (every #'integerp integers) () "Every grid address must be an integer")
  (address-object-from-constants integers)

  )


(defun grid!! (&rest integer-pvars)

  "Returns an ADDRESS-OBJECT pvar.  If the coordinates
   define a virtual processor in the current VP SET,
   the address object will contain the geometry of
   that VP SET.  Otherwise a suitable geometry is
   is either found or created.

   In processors which are not selected, this function
   sets the geometry to be the same geometry as in
   those that are selected, and makes it look like
   the indices provided were all 0.
  "

  (when (null integer-pvars) (error "You cannot have zero addresses.  Sorry."))

  (simple-pvar-argument!! &rest integer-pvars)

  (let ((address-object-pvar
	  (if (every #'pvar-constant-value integer-pvars)
	      (progn
		(assert
		  (every #'(lambda (pvar) (integerp (pvar-constant-value pvar))) integer-pvars) ()
		  "Every grid address must be an integer"
		  )
		(pvar-address-object-from-constant-pvars!! integer-pvars)
		)
	      (pvar-address-object-from-non-constant-pvars!! integer-pvars)
	      )))
    (initialize-address-object-in-non-active-processors address-object-pvar)
    address-object-pvar
    ))




(defun grid-relative!! (&rest integer-pvars)

  "Returns an ADDRESS-OBJECT pvar.  Similar to
   GRID!! except that the coordinate pvars are
   interpreted relative to each processors'
   grid self address.
  "

  (when (null integer-pvars) (error "You cannot have zero addresses.  Sorry."))

  (simple-pvar-argument!! &rest integer-pvars)

  (assert (eql (length integer-pvars) (vp-set-geometry-rank *current-vp-set*)) ()
	  "There are ~D dimensions in the current VP SET, but you provided ~D arguments to GRID-RELATIVE!!"
	  (vp-set-geometry-rank *current-vp-set*) (length integer-pvars)
	  )

  (let ((address-object-pvar
	  (if (every #'pvar-constant-value integer-pvars)
	      (progn
		(assert
		  (every #'(lambda (pvar) (integerp (pvar-constant-value pvar))) integer-pvars) ()
		  "Every grid address must be an integer"
		  )
		(pvar-address-object-from-constant-relative-pvars!! integer-pvars nil *current-vp-set*)
		)
	      (pvar-address-object-from-relative-pvars!! integer-pvars nil *current-vp-set*)
	      )))
    (initialize-address-object-in-non-active-processors address-object-pvar)
    address-object-pvar
    ))


(defun vp-grid-relative!! (vp-set &rest integer-pvars)

  "Returns an ADDRESS-OBJECT pvar in the geometry of the VP SET
   specified by vp-set.  If the coordinates do not fit an
   error is signalled.
  "

  (simple-pvar-argument!! &rest integer-pvars)

  (let ((*current-vp-set* vp-set))
    (let ((address-object (apply #'grid-relative!! integer-pvars)))
      (if (not (eql (vp-set-geometry-id vp-set) (pvar-address-object-geometry-id address-object)))
	  (when (smash-address-object-to-new-vp-set!! address-object vp-set)
	    (error "Internal error.  smash-address-object-to-new-vp-set!! returned and did not error.")
	    )
	  address-object
	  ))))


(defun initialize-address-object-in-non-active-processors (address-object-pvar)
  (*locally
    (declare (type (pvar address-object) address-object-pvar))
    (*compile-blindly
      (*let (not-active)
	(declare (type (pvar boolean) not-active))
	(declare (return-pvar-p nil))
	(*all (declare (return-pvar-p nil)) (*set not-active t!!))
	(*set not-active nil!!)
	(*all
	  (declare (return-pvar-p nil))
	  (*when not-active
	    (declare (return-pvar-p nil))
	    (*setf (address-object-geometry-id!! address-object-pvar) (!! (the fixnum *illegal-geometry-id*)))
	    (*setf (address-object-cube-address!! address-object-pvar) (!! 0))
	    ))
	nil
	))))


(defun pvar-address-object-from-constants!! (coordinates)

  ;; Returns an ADDRESS-OBJECT pvar.

  (let* ((address-object (address-object-from-constants coordinates))
	 (address-object-pvar (!! address-object))
	 )
    (set-address-object-cached-geometry-id address-object-pvar (address-object-geometry-id address-object))
    address-object-pvar
    ))


(defun address-object-from-constants (coordinates)
  (let ((geometry (find-geometry-that-fits-coordinates coordinates)))
    (when (geometry-dimension-offsets geometry)
      (setq coordinates (mapcar #'+ coordinates (geometry-dimension-offsets geometry)))
      )
    (make-address-object
      :geometry-id (geometry-id geometry)
      :cube-address (translate-list-of-grid-coordinates-into-cube-address coordinates geometry nil)
      )))


(defun pvar-address-object-from-constant-pvars!! (constant-pvar-coordinates)
  (let ((constants-list (put-constant-values-from-pvars-into-list constant-pvar-coordinates)))
    (pvar-address-object-from-constants!! constants-list)
    ))


(defun pvar-address-object-from-non-constant-pvars!! (pvar-coordinates)
  
  ;; Returns an ADDRESS-OBJECT pvar.
  
  (pvar-address-object-from-pvar-array-of-grid-addresses
    (pvar-array-of-grid-coordinates-from-list-of-pvar-grid-coordinates pvar-coordinates)
    (length pvar-coordinates)
    ))


(defun pvar-address-object-from-constant-relative-pvars!! (pvar-coordinates wrapped vp-set)
  (pvar-address-object-from-relative-pvars!! pvar-coordinates wrapped vp-set)
  )


(defun-wcefi pvar-address-object-from-relative-pvars!! (pvar-coordinates wrapped vp-set)

  ;; We compute grid indices relative to the given VP SET's processors'
  ;; grid self-addresses.  We need to translate the coordinates into
  ;; absolute addresses.

  (let ((geometry (vp-set-geometry vp-set))
	(length (length pvar-coordinates))
	)

    (*let (temp-relative-coordinate-pvar temp-absolute-grid-coordinate absolute-coordinates-array)
      (declare (type max-grid-address-pvar-type temp-relative-coordinate-pvar temp-absolute-grid-coordinate))
      (declare (type (grid-address-pvar-array-type length) absolute-coordinates-array))

      ;; For each coordinate...

      (do ((relative-pvar-coordinate-list pvar-coordinates (cdr relative-pvar-coordinate-list))
	   (dimension-list (geometry-dimensions geometry) (cdr dimension-list))
	   (j 0 (1+ j))
	   )
	  ((null relative-pvar-coordinate-list))

	(let ((relative-coordinate-pvar (car relative-pvar-coordinate-list)) (dimension-limit (car dimension-list)))
	  
	  ;; Convert the coordinate by force into a signed pvar.

	  (coerce-integer-pvar-into-dest temp-relative-coordinate-pvar relative-coordinate-pvar)

	  ;; Convert the relative coordinate into an absolute coordinate,
	  ;; wrapping if specified.  If wrapping is not specified we may
	  ;; end up with coordinates off the current grid.

	  (*set temp-absolute-grid-coordinate
		(+!! temp-relative-coordinate-pvar 
		     (self-address-grid!! (!! (the fixnum j)))
		     ))
	  (when wrapped
	    (*set temp-absolute-grid-coordinate 
		  (mod!! temp-absolute-grid-coordinate (!! (the fixnum dimension-limit)))
		  ))
	  (*setf (aref!! absolute-coordinates-array (!! (the fixnum j))) temp-absolute-grid-coordinate)

	  ))

      (pvar-address-object-from-pvar-array-of-grid-addresses absolute-coordinates-array length)

      )))


(defun-wcefi add-offsets-to-pvar-array-of-grid-coordinates (pvar-array-of-grid-coordinates geometry)
  (let ((offsets (geometry-dimension-offsets geometry)))
    (when offsets
      (*map
	#'(lambda (coordinate-pvar)
	    (let ((offset (pop offsets)))
	      (when (not (zerop offset))
		(*set (the max-grid-address-pvar-type coordinate-pvar)
		      (+!! (the max-grid-address-pvar-type coordinate-pvar) (!! (the fixnum offset)))
		      ))))
	pvar-array-of-grid-coordinates
	))))


(defun-wcefi subtract-offsets-to-pvar-array-of-grid-coordinates (pvar-array-of-grid-coordinates geometry)
  (let ((offsets (geometry-dimension-offsets geometry)))
    (when offsets
      (*map
	#'(lambda (coordinate-pvar)
	    (let ((offset (pop offsets)))
	      (when (not (zerop offset))
		(*set (the max-grid-address-pvar-type coordinate-pvar)
		      (-!! (the max-grid-address-pvar-type coordinate-pvar) (!! (the fixnum offset)))
		      ))))
	pvar-array-of-grid-coordinates
	))))


(defun add-offsets-to-array-of-grid-coordinates (array-of-grid-coordinates geometry)
  (let ((offsets (geometry-dimension-offsets geometry)))
    (when offsets
      (dotimes (j (geometry-rank geometry))
	(let ((offset (pop offsets)))
	  (when (not (zerop offset))
	    (setf (aref array-of-grid-coordinates j) (+ (aref array-of-grid-coordinates j) offset))
	    ))))))


(defun subtract-offsets-to-array-of-grid-coordinates (array-of-grid-coordinates geometry)
  (let ((offsets (geometry-dimension-offsets geometry)))
    (when offsets
      (dotimes (j (geometry-rank geometry))
	(let ((offset (pop offsets)))
	  (when (not (zerop offset))
	    (setf (aref array-of-grid-coordinates j) (- (aref array-of-grid-coordinates j) offset))
	    ))))))


(*proclaim '(ftype (function (t t) (pvar address-object)) pvar-address-object-from-pvar-array-of-grid-addresses))

(defun-wcefi pvar-address-object-from-pvar-array-of-grid-addresses (array-of-grid-coordinates length)

  (*locally
    (declare (type (grid-address-pvar-array-type length) array-of-grid-coordinates))
    
    (let ((maxs (get-temporary-list-of-length-n length)) min max)
      
      ;; Get the min and max of each pvar coordinate
      
      (do ((j 0 (1+ j)) (maxlist maxs (cdr maxlist)))
	  ((null maxlist))
	
	(setq min (*min (aref!! array-of-grid-coordinates (!! (the fixnum j)))))
	(setq max (*max (aref!! array-of-grid-coordinates (!! (the fixnum j)))))
	
	;; If there are no processors active we just return
	;; an unitialized address object.
	
	(when (null min)
	  (let ((address-object-pvar (make-address-object!!)))
	    (set-address-object-cached-geometry-id address-object-pvar nil)
	    (return-from pvar-address-object-from-pvar-array-of-grid-addresses address-object-pvar)
	    ))
	
	;; if the minimum value is not negative, then
	;; all the values for this coordinate pvar are
	;; non-negative, so there will be no offset for
	;; this dimension, so the maximum value is what
	;; we want.
	
	;; if the minimum value is negative, then if
	;; the minimum is bigger in absolute terms than
	;; the maximum we use that, otherwise we use
	;; the negation of the maximum.  The value stored
	;; must be negative to indicate that an offset
	;; is necessary.
	
	(setf (car maxlist)
	      (if (not (minusp min))
		  max
		  (if (> (abs min) max)
		      min
		      (- (1+ max))
		      ))))
      
      ;; Now we have a bound on the range of each dimension.
      ;; Pass these bounds to the routine which makes
      ;; geometries, and then make an address object using
      ;; this (possibly new) geometry.
      
      (let ((geometry (find-geometry-that-fits-coordinates maxs)))

	; (format t "~%New geometry in PVAR-ADDRESS-OBJECT-FROM-PVAR-ARRAY-OF-GRID-ADDRESSES")
	; (describe geometry)

	;; If the geometry has offsets, add the offsets on to
	;; the coordinates before translating into a cube address.

	(add-offsets-to-pvar-array-of-grid-coordinates array-of-grid-coordinates geometry)

	(let ((pvar-address-object
		(make-address-object!!
		  :geometry-id (!! (the fixnum (geometry-id geometry)))
		  :cube-address
		  (translate-pvar-array-of-grid-coordinates-into-pvar-cube-address!!
			   array-of-grid-coordinates geometry nil
			   ))))
	  (set-address-object-cached-geometry-id pvar-address-object (geometry-id geometry))
	  pvar-address-object
	  ))
      
      )))


(defun address-object-from-array-of-grid-addresses (array-of-grid-coordinates)
  (let ((geometry (find-geometry-that-fits-coordinates (concatenate 'list array-of-grid-coordinates))))
    (add-offsets-to-array-of-grid-coordinates array-of-grid-coordinates geometry)
    (make-address-object
      :geometry-id (geometry-id geometry)
      :cube-address (translate-array-of-grid-coordinates-into-cube-address array-of-grid-coordinates geometry nil)
      )))


(defun-wcefi pvar-array-of-grid-coordinates-from-list-of-pvar-grid-coordinates (grid-coordinates)
  (let ((length (length grid-coordinates)))
    (*let (array-of-grid-coordinates)
      (declare (type (grid-address-pvar-array-type length) array-of-grid-coordinates))
      nil
      (*map
	#'(lambda (grid-array-element)
	    (let ((coordinate (pop grid-coordinates)))
	      nil
	      (coerce-integer-pvar-into-dest grid-array-element coordinate :dest-type max-grid-address-pvar-type)
	      ))
	array-of-grid-coordinates
	)
      array-of-grid-coordinates
      )))



(defun translate-list-of-grid-coordinates-into-cube-address (list-of-grid-coordinates geometry wrapped?)

  (if (and (geometry-dimension-offsets geometry) wrapped?)
      (error "internal error.  Translation to a geometry with offsets and also with instruction to wrap")
      )

  #+*LISP-SIMULATOR

  (let ((cube-address 0))      
    (do ((coordinates-list list-of-grid-coordinates (cdr coordinates-list))
	 (dimensions (geometry-dimensions geometry) (cdr dimensions))
	 (dimension-subsizes (geometry-dimension-subsizes geometry) (cdr dimension-subsizes))
	 (dimension-offsets (geometry-dimension-offsets geometry) (cdr dimension-offsets))
	 )
	((null coordinates-list))
      (let ((offset (if dimension-offsets (car dimension-offsets) 0))
	    (index (if wrapped? (mod (car coordinates-list) (car dimensions)) (car coordinates-list)))
	    )
	(incf cube-address (* (+ offset index) (car dimension-subsizes)))
	))
    cube-address
    )

  #+*LISP-HARDWARE

  (let ((result 0))
    (loop with cm-id-geometry = (geometry-cm-id geometry)
	  for axis from 0
	  for coordinate in list-of-grid-coordinates
	  for geometry-dimension in (geometry-dimensions geometry)
	  do
      
      (cond ((or (>= coordinate geometry-dimension)
		 (minusp coordinate))
	     (if (not wrapped?) (error "The set of grid coordinates ~a will not fit into geometry ~a"
				       list-of-grid-coordinates geometry))
	     (let ((coordinate (mod coordinate geometry-dimension)))
	       (setq result (cm:fe-deposit-news-coordinate cm-id-geometry result axis coordinate))))
	    
	    (t
	     (setq result (cm:fe-deposit-news-coordinate cm-id-geometry result axis coordinate)))))
    result
    )

  )


(defun translate-constant-grid-coordinate-pvars-into-pvar-cube-address!! (constant-grid-coordinate-pvars geometry wrapped)
  (!! (translate-list-of-grid-coordinates-into-cube-address
	geometry
	(put-constant-values-from-pvars-into-list constant-grid-coordinate-pvars)
	wrapped
	)))


#+*LISP-HARDWARE
(defun-wcefi translate-pvar-array-of-grid-coordinates-into-pvar-cube-address!!
	     (pvar-array-of-grid-coordinates geometry wrapped? &optional (allow-error-p t))
  
  ;; Translates a pvar array whose element type
  ;; is max-grid-address-pvar-type into a cube
  ;; address pvar.  The pvar returned is of type
  ;; max-cube-address-pvar-type.	
  
  (let ((array-length (array-pvar-dimension pvar-array-of-grid-coordinates 0)))
    
    (*let-no-bindings ((result (!! 0)) coordinate)
      (declare (type (grid-address-pvar-array-type array-length) pvar-array-of-grid-coordinates))
      (declare (type (field-pvar #.(length-pvar-type 'max-cube-address-pvar-type)) result)
	       (type (signed-pvar #.(length-pvar-type 'max-grid-address-pvar-type)) coordinate)
	       (return-pvar-p t))
      nil

      (loop with geometry-id = (geometry-cm-id geometry)
	    for i below array-length
	    for axis from 0
	    for geometry-dimension in (geometry-dimensions geometry)
	    do
	
	nil
	(*set coordinate (aref!! pvar-array-of-grid-coordinates (!! (the fixnum i))))
	
	(cond
	  ((and wrapped?
		(or (>= (*max (the-signed* coordinate)) geometry-dimension)
		    (minusp (*min (the-signed* coordinate)))))
	   (*let ((pvar (mod!! (the-signed* coordinate) (!! (the fixnum geometry-dimension)))))
	     (declare (type (field-pvar (pvar-length result)) pvar)
		      (return-pvar-p nil))
	     (cm:deposit-news-coordinate-1l
	       geometry-id
	       (pvar-location result)
	       axis
	       (pvar-location pvar)
	       (pvar-length pvar)
	       )))
	  
	  ((and allow-error-p
		(not wrapped?)
		(> *interpreter-safety* 0)
		(*or t!!)
		(or (>= (*max (the-signed* coordinate)) geometry-dimension)
		    (minusp (*min (the-signed* coordinate)))))
	   (error "The ~Dth grid coordinate pvar ~a whose *max is ~D and whose *min is ~D, is attempting to ~
                     access off a grid of dimensions ~A"
		  i coordinate (*max (the-signed* coordinate)) (*min (the-signed* coordinate)) (geometry-dimensions geometry)
		  ))
	  (t
	   (cm:deposit-news-coordinate-1l
	     geometry-id
	     (pvar-location result)
	     axis
	     (pvar-location coordinate) 
	     (pvar-length coordinate)))))
      
      result)))

#+*LISP-SIMULATOR

(defun-wcefi translate-pvar-array-of-grid-coordinates-into-pvar-cube-address!!
	     (pvar-array-of-grid-coordinates geometry wrapped? &optional (allow-error-p t))

  (assert (= (*array-total-size pvar-array-of-grid-coordinates) (geometry-rank geometry)) () "Internal error")

  (*let ((result (!! 0)) coordinate)
    (do ((dimension-index 0 (1+ dimension-index))
	 (subsizes (geometry-dimension-subsizes geometry) (cdr subsizes))
	 (dimension-limits (geometry-dimensions geometry) (cdr dimension-limits))
	 )
	((null subsizes))
      (let ((subsize (car subsizes))
	    (dimension-limit (car dimension-limits))
	    )
	(*set coordinate (aref!! pvar-array-of-grid-coordinates (!! dimension-index)))
	(when wrapped? (*set coordinate (mod!! coordinate (!! dimension-limit))))
	(when allow-error-p
	  (when (and (*or t!!) (or (>= (*max coordinate) dimension-limit) (minusp (*min coordinate))))
	    (error "The grid coordinate pvar for dimension ~D, whose maximum value is ~D~@
                    and whose minimum values is ~D, references off the grid whose dimensions are ~S"
		   dimension-index (*max coordinate) (*min coordinate) (geometry-dimensions geometry)
		   )))
	(*set result (+!! result (*!! coordinate (!! subsize))))
	))
    result
    )

  )


#+*LISP-SIMULATOR

(defun translate-array-of-grid-coordinates-into-cube-address

       (array-of-grid-coordinates geometry wrapped?)

  ;; Front end equivalent.

  (assert (= (array-rank array-of-grid-coordinates) (geometry-rank geometry)) () "Internal error")

  (let ((result 0) coordinate)
    (do ((dimension-index 0 (1+ dimension-index))
	 (subsizes (geometry-dimension-subsizes geometry) (cdr subsizes))
	 (dimension-limits (geometry-dimensions geometry) (cdr dimension-limits))
	 )
	((null subsizes))
      (let ((subsize (car subsizes))
	    (dimension-limit (car dimension-limits))
	    )
	(setq coordinate (aref array-of-grid-coordinates dimension-index))
	(when wrapped? (setq coordinate (mod coordinate dimension-limit)))
	(when (or (>= coordinate dimension-limit) (minusp coordinate))
	  (error "The grid coordinate for dimension ~D, whose value is ~S~@
                    references off the grid whose dimensions are ~S"
		 dimension-index coordinate (geometry-dimensions geometry)
		 ))
	(setq result (+ result (* coordinate subsize)))
	))
    result
    )

  )


;;; This is a hack to avoid consing.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *array-of-temp-lists*)
  (setq *array-of-temp-lists* (make-array *maximum-number-of-dimensions*))
  (dotimes (j *maximum-number-of-dimensions*)
    (setf (aref *array-of-temp-lists* j) (make-list j))
    )

  )


(defun get-temporary-list-of-length-n (length)
  (if (< length *maximum-number-of-dimensions*)
      (aref *array-of-temp-lists* length)
      (error "Implementation limitation.  Only ~D dimensions are allowed currently." *maximum-number-of-dimensions*)
      ))


(defun put-constant-values-from-pvars-into-list (integer-pvars)
  (let ((length (length integer-pvars)))
    (let ((list (get-temporary-list-of-length-n length)))
      (do ((pvars integer-pvars (cdr pvars)) (templist list (cdr templist)))
	  ((null pvars))
	(setf (car templist) (pvar-constant-value (car pvars)))
	)
      list
      )))


(defun-wcefi *cache-id (address-object-pvar &key (geometry-id nil) (ignore-non-active-processors t))
  
  ;; If a geometry-id was provided, just use it, regardless.
  ;; Otherwise see if the pvar's geometry-id slot is
  ;; constant across all processors, or just all active
  ;; processors if so specified.
  
  (*locally
    (declare (type (pvar address-object) address-object-pvar))
    
    (if geometry-id
	(set-address-object-cached-geometry-id address-object-pvar geometry-id)
	(let ((min-geometry-id
		(if ignore-non-active-processors
		    (*min (address-object-geometry-id!! address-object-pvar))
		    (*all (*min (address-object-geometry-id!! address-object-pvar)))
		    ))
	      (max-geometry-id
		(if ignore-non-active-processors
		    (*max (address-object-geometry-id!! address-object-pvar))
		    (*all (*max (address-object-geometry-id!! address-object-pvar)))
		    ))
	      )
	  (if (not (eql min-geometry-id max-geometry-id))
	      (error "The address object you want to cache contains more than one geometry id")
	      (if (null min-geometry-id)
		  nil
		  (set-address-object-cached-geometry-id address-object-pvar geometry-id)
		  ))))))




;;;; From here on is the stuff needed to implement ADDRESS-NTH,
;;;; ADDRESS-NTH!!, ADDRESS-RANK and ADDRESS-RANK!!.


(defun address-nth (address-object dimension)
  (let ((geometry (geometry-from-id (address-object-geometry-id address-object)))
	(cube-address (address-object-cube-address address-object))
	)
    (when (not (and (integerp dimension) (>= dimension 0) (< dimension (geometry-rank geometry))))
      (error "The dimension argument to address-nth, ~S, is not an integer between 0 and ~D exclusive"
	     dimension (geometry-rank geometry)
	     ))
    (grid-address-from-cube-address cube-address geometry dimension)
    ))



#+*LISP-SIMULATOR

(defun grid-address-from-cube-address (cube-address geometry dimension)
  (grid-index-from-row-major-index cube-address geometry dimension)
  )


(defun-wcefi address-nth!! (address-object-pvar dimension-pvar)

  (simple-pvar-argument!! (address-object-pvar dimension-pvar))
  
  (*let (grid-coordinate cube-address)
    (declare (type max-grid-address-pvar-type grid-coordinate))
    (declare (type max-cube-address-pvar-type cube-address))
    
    (*set cube-address (the max-cube-address-pvar-type (address-object-cube-address!! address-object-pvar)))
    
    ; (ppp cube-address :end 10 :title "CUBE ADDRESS IN ADDRESS-NTH!!")

    ;; Do the expected, optimized case fast.
    
    (let ((geometry-id (address-object-cached-geometry-id address-object-pvar))
	  (dimension-constant (pvar-constant-value dimension-pvar))
	  )
      
      (if (and geometry-id (integerp dimension-constant) (>= dimension-constant 0))
	  
	  (progn
	    (*set grid-coordinate
		  (grid-address-pvar-from-cube-address-pvar!!
		    cube-address 
		    (geometry-from-id geometry-id)
		    dimension-constant
		    )))
	  
	  ;; Otherwise map over all the geometries and over all
	  ;; the dimensions specified.

	  (*let (temp-dimension-pvar)
	    (declare (type (field-pvar 32) temp-dimension-pvar))
	    (declare (return-pvar-p nil))
	    (coerce-integer-pvar-into-dest temp-dimension-pvar dimension-pvar)
	    (*map-geometries
	      #'(lambda (geometry-id)
		  (*map-dimensions
		    #'(lambda (dimension-index)
			(*set grid-coordinate
			      (grid-address-pvar-from-cube-address-pvar!!
				cube-address (geometry-from-id geometry-id) dimension-index
				)))
		    temp-dimension-pvar
		    ))
	      address-object-pvar
	      ))
	  
	  ))
    
    grid-coordinate
    
    ))



(defun on-grid!! (address-object) (internal-on-grid!! *current-vp-set* address-object))

(defun vp-on-grid!! (vp-set address-object) (internal-on-grid!! vp-set address-object))


(defun-wcefi internal-on-grid!! (vp-set address-object)
  (without-void-pvars (address-object)
    (*let ((on-grid? (=!! (address-rank!! (the (pvar address-object) address-object))
			  (!! (the fixnum (vp-set-geometry-rank vp-set)))
			  ))
	   grid-address
	   )
      (declare (type (pvar boolean) on-grid?))
      (declare (type max-grid-address-pvar-type grid-address))
      (declare (return-pvar-p t))
      (do ((dimensions-list (vp-set-geometry-dimensions vp-set) (cdr dimensions-list))
	   (index 0 (1+ index))
	   )
	  ((null dimensions-list))
	(let ((dimension-limit (car dimensions-list)))
	  (*set grid-address (address-nth!! (the (pvar address-object) address-object) (!! (the fixnum index))))
	  (*set (the boolean-pvar on-grid?)
		(and!! (the boolean-pvar on-grid?)
		       (<!! (!! -1) grid-address
			    (!! (the fixnum dimension-limit))
			    )))))
      on-grid?
      )))



(defun address-rank (address-object)
  (geometry-rank (geometry-from-id (address-object-geometry-id address-object)))
  )



(defun-wcefi address-rank!! (address-object-pvar)

  (simple-pvar-argument!! address-object-pvar)

  (*let (rank)
    (declare (type geometry-rank-pvar-type rank))
    (*map-geometries
      #'(lambda (geometry-id) (*set rank (!! (the fixnum (geometry-rank (geometry-from-id geometry-id))))))
      address-object-pvar
      )
    rank
    ))


(defun-wcefi *single-rank (address-object-pvar)
  (let ((geometry-id (address-object-cached-geometry-id address-object-pvar)))
    (if geometry-id
	(geometry-rank (geometry-from-id geometry-id))
	(*let ((rank!! (address-rank!! (the (pvar address-object) address-object-pvar))))
	  (declare (type geometry-rank-pvar-type rank!!))
	  (let ((min-rank (*min rank!!)) (max-rank (*max rank!!)))
	    (assert (or (null min-rank) (= min-rank max-rank)) ()
		    "Error:  Grids of different ranks are encoded in the address-object-pvar, but you want to perform ~
                     an operation that demands that the address-object-pvar encode addresses of the same rank"
		    )
	    min-rank
	    )))))


(defun-wcefi address-plus-nth!! (address-object-pvar increment-pvar dimension-pvar &key (wrapped? nil))
  
  (simple-pvar-argument!! (address-object-pvar increment-pvar dimension-pvar))
  
  (*locally
    (declare (type (pvar address-object) address-object-pvar))

    ;; Make sure all the addresses have the same ranks
  
    (let ((rank (*single-rank address-object-pvar)))
    
      ;; if there are no processors active there is nothing to do.
      ;; Return the old address-object-pvar.
    
      (if (null rank)
	
	  address-object-pvar
	
	  ;; Otherwise create the new address-object-pvar to return,
	  ;; and make an array consisting of the grid coordinates
	  ;; of the old address-object-pvar.
	
	  (*let (new-address-object-pvar temp-grid-coordinates-array cube-address-pvar temp-increment temp-dims)
	    (declare (type (pvar address-object) new-address-object-pvar))
	    (declare (type (grid-address-pvar-array-type rank) temp-grid-coordinates-array))
	    (declare (type max-cube-address-pvar-type cube-address-pvar))
	    (declare (type (pvar (signed-byte (1+ (pvar-length increment-pvar)))) temp-increment))
	    (declare (type (pvar (unsigned-byte #.*max-cube-address-length*)) temp-dims))
	    (declare (return-pvar-p t))
	    nil
	  
	    (coerce-integer-pvar-into-dest temp-increment increment-pvar)
	    (coerce-integer-pvar-into-dest temp-dims dimension-pvar)
	    (*set cube-address-pvar (address-object-cube-address!! address-object-pvar))
	  
	    (*map-geometries
	    
	      #'(lambda (geometry-id)
		
		  (store-pvar-grid-addresses-from-pvar-cube-address-into-array
		    cube-address-pvar (geometry-from-id geometry-id) temp-grid-coordinates-array
		    )
		
		  ;; for each dimension present
		  ;; add the increment for that dimension to the grid coordinate.
		
		  (*map-dimensions
		    #'(lambda (dimension-index)
			(*setf (aref!! temp-grid-coordinates-array (!! (the fixnum dimension-index)))
			       (+!! (aref!! temp-grid-coordinates-array (!! (the fixnum dimension-index)))
				    temp-increment
				    ))
			(when wrapped?
			  (*setf (aref!! temp-grid-coordinates-array (!! (the fixnum dimension-index)))
				 (mod!! (aref!! temp-grid-coordinates-array (!! (the fixnum dimension-index)))
					(!! (the fixnum (nth dimension-index
							     (geometry-dimensions (geometry-from-id geometry-id))
							     )))))))
		    temp-dims
		    )
		
		  (*set new-address-object-pvar
			(pvar-address-object-from-pvar-array-of-grid-addresses
			  temp-grid-coordinates-array rank
			  )))
	    
	    
	      address-object-pvar
	    
	      )
	  
	    (when wrapped?
	      (assert
		(*and (=!! (address-object-geometry-id!! new-address-object-pvar)
			   (address-object-geometry-id!! address-object-pvar)
			   ))
		()
		"Internal error.  :wrapped but didn't get the same geometry(s) back"
		))

	    (when wrapped?
	      (setf (pvar-address-object-geometry-id new-address-object-pvar)
		    (pvar-address-object-geometry-id address-object-pvar)
		    ))

	    new-address-object-pvar
	  
	    )
	
	  ))))




#+*LISP-SIMULATOR

(defun store-pvar-grid-addresses-from-pvar-cube-address-into-array (cube-address-pvar geometry pvar-array)
  
  ;; Extracts each grid addresses and stores the
  ;; jth grid address so extracted into the jth
  ;; element of pvar-array.  The element type
  ;; of pvar-array is max-grid-address-pvar-type.
  
  (let ((dimension-index 0))
    
    (*map
     #'(lambda (array-element)
         (let ((array-element-array (pvar-array array-element))
               (cube-address-array (pvar-array cube-address-pvar))
               )
           (with-simple-vectors (array-element-array cube-address-array)
             (do-for-selected-processors-internal (j)
               (setf (aref array-element-array j) 
                 (grid-index-from-row-major-index (aref cube-address-array j) geometry dimension-index)
                 ))
             (incf dimension-index)
             )))
     pvar-array
     )
    
    (subtract-offsets-to-pvar-array-of-grid-coordinates pvar-array geometry)
    
    ))






#+*LISP-SIMULATOR

(defun store-grid-addresses-from-cube-address-into-array (cube-address geometry array)

  ;; Extracts each grid address and stores the
  ;; jth grid address into the jth element of
  ;; array.

  (dotimes (dimension-index (geometry-rank geometry))
    (setf (aref array dimension-index) (grid-index-from-row-major-index cube-address geometry dimension-index))
    )

  (subtract-offsets-to-array-of-grid-coordinates array geometry)

  )



(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *temp-arrays-for-addressing*)
(setq *temp-arrays-for-addressing* (make-array *maximum-number-of-dimensions*))
(dotimes (j *maximum-number-of-dimensions*)
  (setf (aref *temp-arrays-for-addressing* j) (make-array j))
  )

)


(defun get-temp-array-of-length-n (n)
  (when (>= n *maximum-number-of-dimensions*)
    (error "Implementation limitation.  Only ~D dimensions are currently allowed." *maximum-number-of-dimensions*)
    )
  (aref *temp-arrays-for-addressing* n)
  )



(defun address-plus-nth (address-object increment dimension &key (wrapped? nil))
  (let ((geometry-id (address-object-geometry-id address-object))
	(cube-address (address-object-cube-address address-object))
	)
    (let* ((rank (geometry-rank (geometry-from-id geometry-id)))
	   (temp-array (get-temp-array-of-length-n rank))
	   )
      (assert (< -1 dimension rank) ()
	      "The address object ~S has ~D dimensions, but you want to increment dimension ~D"
	      address-object rank dimension
	      )
      (store-grid-addresses-from-cube-address-into-array cube-address (geometry-from-id geometry-id) temp-array)
      (setf (aref temp-array dimension) (+ (aref temp-array dimension) increment))
      (when wrapped?
	(setf (aref temp-array dimension)
	      (mod (aref temp-array dimension) (nth dimension (geometry-dimensions (geometry-from-id geometry-id))))
	      ))
      (let ((result (address-object-from-array-of-grid-addresses temp-array)))
	(when wrapped?
	  (assert (= (address-object-geometry-id result) (address-object-geometry-id address-object))
		  ()
		  "Internal error.  :wrapped was specified but a different geometry was returned"
		  ))
	result
	))))





(defun-wcefi *map-geometries (function address-object-pvar)

  (if (address-object-cached-geometry-id address-object-pvar)

      (funcall function (address-object-cached-geometry-id address-object-pvar))

      (*let ((geometry (the (field-pvar *) (address-object-geometry-id!! address-object-pvar)))
	     (active t!!)
	     )
	(declare (type geometry-id-pvar-type geometry))
	(declare (type (pvar boolean) active))
	nil
	(with-css-saved
	  (block foo
	    (loop
	      (*when active
		(let ((min (*min geometry)))
		  (if (null min)
		      (return-from foo nil)
		      (*when (=!! (!! (the fixnum min)) geometry)
			(funcall function min)
			(*set active nil!!)
			)))
		nil
		))))
	nil
	)))




(defun-wcefi *map-dimensions (function unsigned-pvar)

  (*locally
    (declare (type (pvar (unsigned-byte (pvar-length unsigned-pvar))) unsigned-pvar))

    (if (pvar-constant-value unsigned-pvar)

	(funcall function (pvar-constant-value unsigned-pvar))

	(*let ((active t!!))
	  (declare (type (pvar boolean) active))
	  nil
	  (with-css-saved
	    (block foo
	      (loop
		(*when active
		  (let ((min (*min unsigned-pvar)))
		    (if (null min)
			(return-from foo nil)
			(*when (=!! (!! (the fixnum min)) unsigned-pvar)
			  (funcall function min)
			  (*set active nil!!)
			  )))
		  nil
		  ))))
	  nil
	  ))))



(defun-wcefi grid-address-pvar-from-cube-address-pvar!! (cube-address-pvar geometry dimension-scalar)
  
  (assert (< -1 dimension-scalar (geometry-rank geometry)) ()
          "The address object only has ~D dimensions, but you asked for dimension ~D"
          (geometry-rank geometry) dimension-scalar
          )
  
  (*let (grid-address-pvar)
        (let ((grid-address-array (pvar-array grid-address-pvar))
              (cube-address-array (pvar-array cube-address-pvar))
              (offset (if (geometry-dimension-offsets geometry) 
                          (elt (geometry-dimension-offsets geometry) 
                               dimension-scalar)))
              )
          (with-simple-vectors (grid-address-array cube-address-array)
          (if (and offset (not (zerop offset)))
              (do-for-selected-processors-internal (j)
                (setf (aref grid-address-array j)
                  (- (grid-index-from-row-major-index (aref cube-address-array j) geometry dimension-scalar) offset)
                  ))
            (do-for-selected-processors-internal (j)
              (setf (aref grid-address-array j)
                (grid-index-from-row-major-index (aref cube-address-array j) geometry dimension-scalar)
                ))))
        grid-address-pvar
        ))
  
  )



;;; Functions to check that coordinates are in range.
;;; All completely straightforward right now.



(defun grid-coordinates-fit-into-geometry-p (geometry scalar-grid-components)
  (do ((dimensions (geometry-dimensions geometry) (cdr dimensions))
       (component-list scalar-grid-components (cdr component-list))
       )
      ((null dimensions))
    (let ((component (car component-list)) (dimension (car dimensions)))
      (if (not (in-range dimension component))
	  (return-from grid-coordinates-fit-into-geometry-p nil)
	  )))
  t
  )



(defun any-grid-coordinate-out-of-range-p (geometry scalar-grid-components)
  (not (grid-coordinates-fit-into-geometry-p geometry scalar-grid-components))
  )


(defun-wcefi pvar-grid-coordinates-fit-into-geometry-p (geometry grid-component-pvars)
  (*let (temp-grid-coordinate)
    (declare (type max-grid-address-pvar-type temp-grid-coordinate))
    nil
    (do ((dimensions (geometry-dimensions geometry) (cdr dimensions))
	 (component-list grid-component-pvars (cdr component-list))
	 )
	((null dimensions))
      (let ((component (car component-list)) (dimension (car dimensions)))
	nil
	(coerce-integer-pvar-into-dest temp-grid-coordinate component)
	(if (not (*and (in-range!! dimension (the max-grid-address-pvar-type component))))
	    (return-from pvar-grid-coordinates-fit-into-geometry-p nil)
	    )))
    t
    ))


(defun-wcefi pvar-array-of-grid-coordinates-fit-into-geometry-p (geometry pvar-array-of-grid-coordinates)
  (*let ()
    (declare (return-pvar-p nil))
    (block exit-block
      (let ((dimensions (geometry-dimensions geometry)))
	(*map
	  #'(lambda (jth-coordinate-pvar)
	      (if (not (*and (in-range!! (car dimensions) (the max-grid-address-pvar-type jth-coordinate-pvar))))
		  (return-from exit-block nil)
		  )
	      (pop dimensions)
	      )
	  pvar-array-of-grid-coordinates
	  ))
      t
      )))


(defun-wcefi pvar-array-of-grid-coordinates-fit-into-geometry-p!! (geometry pvar-array-of-grid-coordinates)
  (let!! ((fits t!!))
    (declare (type (pvar boolean) fits))
    (let ((dimensions (geometry-dimensions geometry)))
      (*map
	#'(lambda (jth-coordinate-pvar)
	    (*set fits (and!! fits (in-range!! (car dimensions) (the max-grid-address-pvar-type jth-coordinate-pvar))))
	    (pop dimensions)
	    )
	pvar-array-of-grid-coordinates
	))
    fits
    ))


(defun any-pvar-grid-coordinate-out-of-range-p (geometry grid-component-pvars)
  (not (pvar-grid-coordinates-fit-into-geometry-p geometry grid-component-pvars))
  )


(defun array-of-grid-coordinates-fit-into-geometry-p (geometry array-of-grid-coordinates)
  (let ((dimensions (geometry-dimensions geometry)))
    (map
      nil
      #'(lambda (coordinate)
	  (if (not (in-range (car dimensions) coordinate))
	      (return-from array-of-grid-coordinates-fit-into-geometry-p nil)
	      (pop dimensions)
	      ))
      array-of-grid-coordinates
      ))
  t
  )
