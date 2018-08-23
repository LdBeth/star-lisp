;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *LISP-I; MUSER: YES -*-

;;;> ************************************************************************************
;;;> Copyright 1990 Thinking Machines Corporation, Inc. of Cambridge, Massachusetts.
;;;> All rights reserved
;;;> ************************************************************************************

(in-package :*lisp-i)

;;; No-Bang-Bang.lisp
;;; By William R. Swanson, with pointers from JP Massar
;;; Created: 3/19/90
;;; Revised: 4/3/90 - I decided to do things more generically. WRS
;;;          8/90 - Added simple-pvar-argument!! for simulator functions. WRS
;;;          9/5 - Added &front-end keyword WRS

;;; The global variables and macros defined in this file allow the *Lisp user to
;;; provide scalar arguments to *Lisp functions that expect pvar arguments.
;;; These scalars are automatically converted to pvars (ala !!), thus obviating
;;; the need to use the !! operator in most expressions.

;;; The main purpose of this feature is to eliminate the need for !! in supplying
;;; arguments to *Lisp functions where some arguments may be constant pvar
;;; expressions (i.e., (+!! (!! 1) (!! 2) (!! 23)...&ad-nauseum))

;;; Conversion is performed by any function or macro in which the forms below are
;;; applied to arguments.

;;; Conversion is NOT performed for operators such as *funcall and *apply, where
;;; it is obvious that some arguments MUST be scalars. Also, conversion is not
;;; performed for any argument to a *Lisp operator that obviously MUST be a
;;; "real" pvar (i.e., pvar-name, pvar-field, etc.), or to an operation where such
;;; conversion would obviously defeat the intended purpose of the operation
;;; (tests such as booleanp!!, characterp!!, etc.)

;;; This variable controls whether the feature is included when *Lisp
;;; is compiled.  

(defvar *convert-scalar-args-to-pvars* t
  "Whether to automatically convert scalar arguments that should be pvars into pvars")

;;; This variable controls whether the feature is active, and may be
;;; altered at run-time. It is inactive by default.

(defvar *convert-scalar-args-p* t
  "Whether to automatically convert scalar arguments (conditional at run-time)")

;;; A simple function to make turning it on and off easy:

(defun no-bang-bang (&optional (enable-feature-p t))
  (setq *convert-scalar-args-to-pvars* enable-feature-p)
  (setq *convert-scalar-args-p* enable-feature-p))

;;; Functions to toggle scalar promotion

(defun disable-scalar-promotion ()
  (setq *lisp-i::*convert-scalar-args-p* nil))

(defun enable-scalar-promotion ()
  (setq *lisp-i::*convert-scalar-args-p* t))

;;; Scalar to Pvar Argument Conversion Macros: 
;;; A call to these macros may be included within any *Lisp operator where
;;; conversion should be applied to an argument. If argument conversion is
;;; disabled, these macros do nothing. (Expand to NIL or pass args unchanged)

;;; Pvar-argument is called in the following way:

;;; (pvar-argument!! <arg-desc1> <arg-desc2> <arg-desc3>...)

;;; where <arg-descn> is one of:
;;;   argument-name data-type        --  useful for single-arg functions
;;;   (argname1 argname2 ...) data-type  -- useful for multi-arg functions
;;;   &opt argument-name data-type   -- makes handling optionals simpler
;;;   &opt (argname1 argname2 ...) data-type  -- useful for multi-arg optionals
;;;   &front-end argument-name data-type   -- for front-end arguments
;;;   &front-end (argname1 argname2 ...) data-type  -- for multiple fe arguments
;;;   &rest argument-name data-type  --  &rest args _must_ be handled seperately

;;; A sample call to pvar-argument!! might look like:
;;; (pvar-argument!! elephantp boolean (ears trunk tail integer) &rest wrinkle-types complex)

(defmacro pvar-argument!! (&rest argument-descriptors)
  (when *convert-scalar-args-to-pvars*
    (let (result class type)
      (do* ((nextarg (when argument-descriptors (pop argument-descriptors))
	             (when argument-descriptors (pop argument-descriptors))))
	   ((null nextarg))
	(if (symbolp nextarg)
	    (case nextarg
	      (&rest (setq class :&rest
			   nextarg (pop argument-descriptors)))
	      (&opt (setq class :&opt
			  nextarg (pop argument-descriptors))
		    (when (listp nextarg)
		      (setq class :&opt-mult)))
	      (&front-end (setq class :&front-end
				nextarg (pop argument-descriptors))
			  (when (listp nextarg)
			    (setq class :&front-end-mult)))
	      (otherwise (setq class :single)))
	    (setq class :multiple))
	(setq type (pop argument-descriptors))
	(multiple-value-bind (declaration test conversion optimization)
	    (conversion-for-type type)
	  (when test ;;; if the type is one we know how to convert
	    (case class
	      (:single
		(push (conversion-form-for nextarg test declaration conversion optimization)
		      result))
	      (:&opt
		(push `(when ,nextarg
			 ,(conversion-form-for nextarg test declaration conversion optimization))
		      result))
	      (:&opt-mult
		(dolist (argname nextarg)
		  (push `(when ,argname
			  ,(conversion-form-for argname test declaration conversion optimization))
			result)))
	      (:&front-end
		(push (conversion-form-for nextarg test declaration conversion optimization t)
		      result))
	      (:&front-end-mult
		(dolist (argname nextarg)
		  (push (conversion-form-for argname test declaration conversion optimization t)
			result)))
	      (:&rest 
		(push (rest-conversion-form nextarg test declaration conversion optimization)
		      result))
	      (:multiple
		(dolist (argname nextarg)
		  (push (conversion-form-for argname test declaration conversion optimization)
			result)))
	      ))))
      (when result
	`(*lisp-i::*nocompile (when *convert-scalar-args-p* ,@(nreverse result)))))))

;;; A macro for simple cases:

(defmacro simple-pvar-argument!! (&rest arglist)
  (labels ((expandify!! 
            (arglist)
            (if (null arglist) NIL
              (if (member (car arglist) '(&opt &rest))
                  (if (null (cdr arglist))
                      (progn (warn "&opt or &rest pvar-argument expression missing argument.") nil)
                    `(,(car arglist) ,(cadr arglist) legal-pvar-value
                        ,@(expandify!! (cddr arglist))))
                `(,(car arglist) legal-pvar-value
                    ,@(expandify!! (cdr arglist)))))))
    (unless (null arglist)
      `(pvar-argument!! ,@(expandify!! arglist)))))


;;; These functions produce conversion forms for the above macros

(defun bang-bang-form-for (argname &optional (declaration nil) (conversion nil) (front-end-p nil))
  (let ((form argname))
    (when conversion
      (setq form `(,conversion ,form)))
    (when declaration
      (setq form `(the ,declaration ,form)))
    (if front-end-p
	(setq form `(front-end!! ,form))
	(setq form `(!! ,form)))))

(defun conversion-form-for (argname test &optional (declaration nil)
				                   (conversion nil)
						   (optimization nil)
						   (front-end-p nil))
  (let ((form
	 `(if (,test ,argname)
	      (setq ,argname ,(bang-bang-form-for argname declaration conversion front-end-p)))))
    (when optimization
      (setq form `(progn (setq ,argname (,optimization ,argname)) ,form)))
    form))

(defun rest-conversion-form (argname test &optional (declaration nil)
				                    (conversion nil)
						    (optimization nil))
  `(do ((%arglist% ,argname (cdr %arglist%)))
       ((null %arglist%))
     ,@(when optimization `((rplaca %arglist% (,optimization (car %arglist%)))))
     (if (,test (car %arglist%))
	 (rplaca %arglist% ,(bang-bang-form-for '(car %arglist%) declaration conversion)))))

;;; This variable contains conversion information for all types
;;; that need to be converted (thus far).

(defvar *conversion-list* nil
    "Scalar-to-pvar conversion info for types that can be converted.")

(setq *conversion-list*
    ;;; Elements of the form: (nametag declaration test [conversion optimization])
    ;;; listed in order of relative frequency of use of each data type.
    ;;; nametag is conversion tag used in pvar-argument!! to determine test needed
    ;;; declaration is how scalar value being converted should be declared
    ;;; test is a boolean test that checks for that scalar type
    ;;; conversion is an optional function that gets called on the scalar value
    ;;;   immediately prior to testing, to possibly convert it into a nicer form.
    ;;; optimization is an optional function that gets called on the scalar value
    ;;;   before (and independant of) testing, to allow possible optimizations that
    ;;;   make test unnecessary.
    '((float float floatp)
      (integer integer integerp)
      (boolean boolean booleanp nil boolean-optimize)
      (boolarg nil boolargp nil boolean-optimize)
      (complex complex complexp)
      (non-complex number non-complexp)
      (character character characterp)
      (number number numberp)
      (legal-pvar-value nil legal-pvar-valuep)
      (vector vector front-end-vector-p)
      (array array front-end-array-p)
      (*defstruct nil *defstructp)
      (*sequence vector *sequencep)
      (sf-vector vector sf-vectorp)
      (bit-array array bit-arrayp)
      (charint nil charintp)
      (char-bitspec integer char-bitspecp bitspec-to-integer)
      ;;; Bytespecs are a pain.
      (*bytespec 
           #-(AND LUCID *LISP-SIMULATOR) integer
           #+(AND LUCID *LISP-SIMULATOR) nil
           *bytespecp nil bytespec-optimize)
      (address-object nil address-object-p)
      (segment-set-object nil segment-set-objectp)
      ))

(defun conversion-for-type (type)
  (let* ((known (member type *conversion-list* :key 'car)))
    (if known
      (setq known (cdr (car known)))
      (warn "Not one of known conversion types: ~A." type))
    (values-list known)))


;;; This used to be in pvars.lisp, but it is used further
;;; down in this file so now its here.

(defmacro fast-pvarp (x) `(internal-pvarp ,x))

;;; For now, only the following types are important:

;;; >>> Boolean: T or NIL

(defun booleanp (thingy)
  (or (eq thingy t) (eq thingy nil)))

;;; Boolean argument may be either a boolean value, or any non-NIL value.

(defun boolargp (thingy)
  (legal-pvar-valuep thingy))

;;; We can optimize by changing to a specific pvar in two cases:

(defun boolean-optimize (thingy)
  (cond ((eq thingy t) t!!)
	((eq thingy nil) nil!!)
	(t thingy)))

;;; >>> Number: numberp exists

;;; >>> Integer: integerp exists

;;; >>> Float: floatp exists

;;; >>> Complex: complexp exists

;;; >>> Non-complex:

(defun non-complexp (thingy)
  (and (numberp thingy) (not (complexp thingy))))

;;; >>> Character: characterp exists

;;; >>> Array: arrayp exists

(defun front-end-array-p (x)
  (and (not (fast-pvarp x)) (arrayp x))
  )

(defun front-end-vector-p (x)
  (and (not (fast-pvarp x)) (vectorp x))
  )

;;; >>> Bit-array: use arrayp, and let bit array functions catch non-bit array pvars

(defun bit-arrayp (thingy)
  (front-end-array-p thingy))

;;; >>> Vector: vectorp exists

;;; >>> Sf-vector: (single-float vector) use vectorp, let (d)sf functions catch non-sf vectors

(defun sf-vectorp (thingy)
  (and (not (fast-pvarp thingy)) (vectorp thingy))
  )
 
;;; >>> *sequence: (*Lisp sequence) use vectorp, since *Lisp sequences can only be vectors

(defun *sequencep (thingy)
  (and (not (fast-pvarp thingy)) (vectorp thingy))
  )

;;; >>> *defstruct: (structure object defined by *defstruct)

(defun *defstructp (thingy)
  (let ((thing-type (type-of thingy)))
    (and (symbolp thing-type)
	 (structure-pvar-type-known-*defstruct-type thing-type))))

;;; >>> legal-pvar-value: (pvar value) Any legal argument to !!

(defun legal-pvar-valuep (thing)
  (or (booleanp thing)      ; t or nil
      (numberp thing)       ; integer, float, complex
      (characterp thing)    ; string-char and character
      (*defstructp thing)   ; *defstructs
      (and (not (fast-pvarp thing)) (arrayp thing)) ; arrays
      ))

;;;(arrayp thing)        ; arrays, vectors/sequences
;;;(*defstructp thing))) ; *defstruct objects

;;; <<< The following are specialized types >>>

;;; >>> Charint: (character or integer)

(defun charintp (thingy)
  (or (integerp thingy) (characterp thingy)))

;;; >>> char-bitspec: (bit specifier for char-bit!!)

(defun char-bitspecp (thingy)
  (member thingy '(0 :control 1 :meta 2 :super 3 :hyper)))

(defun bitspec-to-integer (thingy)
  (if (integerp thingy)
      thingy
      (position thingy '(:control :meta :super :hyper))))

;;; >>> *bytespec: (*Lisp byte-specifier)

;;; This is really ridiculous.
;;; On Lispms, bytespecs are always integers, and *bytespecs are integer pvars
;;; In Lucid, bytespecs are an ugly internal data type, and *bytespecs are:
;;;   on the hardware, integer pvars, since that's the only way to represent them
;;;   in the simulator, "pvars" of the ugly data type

(defconstant *bytespec-type* #+symbolics 'fixnum
	                     #+:CCL (type-of (byte 1 0)) ;; 0 0 is illegal in Allegro
			     #-(OR symbolics :CCL) (type-of (byte 0 0))) ;; this gives the nasty type in Lucid

(defun *bytespecp (thingy)
  (or
    (typep thingy *bytespec-type*)
    #+LUCID (typep thingy 'integer) ;; allow integer bytespecs under Lucid
    ))

;;; This stub keeps the call to constant-byte!! below from returning a warning
;;; at compile-time, since I don't think it really makes sense to move constant-byte!!
;;; into this file.

#+(AND LUCID *LISP-HARDWARE)
(defun constant-byte!! (size position)
  (ignore size position)
  (error "CONSTANT-BYTE!!, defined in no-bang-bang.lisp, was not redefined by *Lisp interpreter."))

;;; Turn Lucid's nasty bytespec structure into a friendly integer (except in simulator!),
;;; so operators requiring bytespec operators will eat calls to CL byte function.
;;; In simulator under Lucid, bytespec pvars are always the nasty type, so
;;; take care of 'em by turning them into the right byte!! call!

(defun bytespec-optimize (thingy)
  #+(AND LUCID *LISP-HARDWARE) ;; only need to convert Lucid bytespecs on hardware
  (if (eql (type-of thingy) *bytespec-type*)
      (setq thingy (constant-byte!! (byte-size thingy) (byte-position thingy))))
  #+(AND LUCID *LISP-SIMULATOR)
  (if (eql (type-of thingy) *bytespec-type*)
      (setq thingy (byte!! (!! (byte-size thingy)) (!! (byte-position thingy)))))
  thingy)

;;; <<< The following two are both *defstructs, so these tests may not be needed>>>

;;; >>> Address-Object: *Lisp-I:address-object-p exists

;;; >>> Segment-Set-Object:

(defun segment-set-objectp (thingy)
  (typep thingy 'segment-set))

;;;; Tests for the above
;
;(defmacro nbtestarm (val type)
;  `(let ((arg ,val))
;     (format t "~S " arg)
;     (pvar-argument!! arg ,type)
;     (format t "~S~%" arg)))
;
;(defun nbtest ()
;  (*let () ; to cleanup stack afterwards
;    (nbtestarm 3.14159 float)
;    (nbtestarm 3 integer)
;    (nbtestarm t boolean)
;    (nbtestarm nil boolean)
;    (nbtestarm 3 boolarg)
;    (nbtestarm #C(3.0 4.0) complex)
;    (nbtestarm 5 non-complex)
;    (nbtestarm #\G character)
;    (nbtestarm 0 number)
;    (nbtestarm 23 legal-pvar-value)
;    (nbtestarm #(1 2 3) vector)
;    (nbtestarm #(1 2 3) array)
;    (nbtestarm #(1 2 3) *sequence)
;    (nbtestarm #(T NIL T) bit-array)
;    (nbtestarm 23 charint)
;    (nbtestarm :control char-bitspec)
;    (nbtestarm 0 char-bitspec)
;    (nbtestarm (grid 2 3) address-object)
;    ))
;
