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


#|

This defines the Starlisp type system.

*PROCLAIM.  

This is a user level macro.  It is used to
tell Starlisp (mostly the Starlisp compiler) various things,
like type information.

Evaluating a *PROCLAIM will have the side effect of
changing various property lists of symbols and internal
lists that Starlisp keeps around.

*DECLAIM 

Like *PROCLAIM, but takes a set of proclamations which
do not have to be quoted.

PROCLAIM, DECLAIM, DEFTYPE

These shadow their Common Lisp counterparts, enabling *Lisp
to record information (mostly type information) provided by
these constructs.  Each ends up calling its Lisp counterpart
eventually.

UNPROCLAIM

This is a user level function.  it is used to undo the
effects of a *PROCLAIM, *DECLAIM, DECLAIM or a PROCLAIM.

STANDARD-TYPE-P

For Starlisp's internal use.  Whether a type name is a
standard Common Lisp type or not.

CANONICAL-TYPE

For Starlisp's internal use.  Returns a symbol or form
which is the canonical representation for the type
specified by the input.

PROCLAIMED-TYPE

Returns the type a symbol has been proclaimed to have.

PROCLAIMED-FTYPE

Returns the function type a symbol has been proclaimed to have.

PROCLAIMED-SPECIAL-P

Returns whether a symbol have been proclaimed special.

PVAR

This is redefined as a Common Lisp deftype.  
The Common Lisp function typep then works
using this deftype definition.

 A canonical type is one of the following:
 (pvar boolean)
 (pvar (unsigned-byte <length>)), where length is *, or an expression.
 (pvar (signed-byte <length>)), where length is *, or an expression.
 (pvar (defined-float <mantissa> <exponent>)), where mantissa and exponent are *, or an expression.
 (pvar (complex (defined-float <mantissa> <exponent>))), where mantissa and exponent are *, or an expression.
 (pvar (array <element-type> <dimensions>)), where element-type is * or a canonical pvar element-type,
      and where dimensions are *, or a list of *, or expressions.
 (pvar (structure <name>)), where name is *, or a symbol that has been defined by *defstruct.
 (pvar string-char)
 (pvar character)
 (pvar front-end)
 (pvar t)
 (pvar *)

CANONICAL-PVAR-TYPE

For Starlisp's internal use.  Returns a symbol or form
which is the canonical representation of the pvar type
specified by the input, or NIL if the input is not
a legal canonical pvar type.

VALID-PVAR-TYPE-P

For Starlisp's internal use.  Same as CANONICAL-PVAR-TYPE
except that it takes an extra optional argument, error,
which defaults to T.  If error is T, then if the type
passed in is not a legal pvar type, the routine will
error out and print out a message as to why the type
was not legal.

LENGTH-PVAR-TYPE

For Starlisp's internal use.  Returns the length in bits
of a type or * if the type specifies an indeterminate
length.

CANONICAL-PVAR-ELEMENT-TYPE

This will return one of boolean, unsigned-byte, signed-byte, defined-float,
complex, string-char, character, array, structure, t, front-end, or *.

MAKE-CANONICAL-PVAR-TYPE

This takes a pvar element type as defined in CANONICAL-TYPE-ELEMENT-TYPE
and other parameters and returns a canonical pvar type.

MAKE-*DEFUN-FUNCTION

Given a symbol foo, this returns another symbol which is the
name of an internal function which would be defined if
*DEFUN were used to define foo.

ARRAY-PVAR-TYPE-P
STRUCTURE-PVAR-TYPE-P
FLOAT-PVAR-TYPE-P
COMPLEX-PVAR-TYPE-P
BOOLEAN-PVAR-TYPE-P
FRONT-END-PVAR-TYPE-P
GENERAL-PVAR-TYPE-P
STRING-CHAR-PVAR-TYPE-P
CHARACTER-PVAR-TYPE-P
SIGNED-PVAR-TYPE-P
UNSIGNED-PVAR-TYPE-P

These all decide if a pvar type is of the
appropriate class.  They only work on
canonical pvar types.

FLOAT-PVAR-TYPE-MANTISSA
FLOAT-PVAR-TYPE-EXPONENT
COMPLEX-PVAR-TYPE-MANTISSA
COMPLEX-PVAR-TYPE-EXPONENT 
ARRAY-PVAR-TYPE-DIMENSIONS
ARRAY-PVAR-TYPE-ELEMENT-TYPE
STRUCTURE-PVAR-TYPE-NAME

These extract parameters from a canonical
pvar type of the proper type.

|#



(declaim (special *speed* *safety* *space* *compilation-speed* *verbose*))


(defmacro *proclaim (decl-spec)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (starlisp-proclaim ,decl-spec)))

(defun *lisp::proclaim (proclamation)
  (starlisp-proclaim proclamation)
  )

(defmacro *declaim (&rest decl-specs)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar #'(lambda (ds) `(starlisp-proclaim ',ds)) decl-specs)
     ))

(defmacro *lisp::declaim (&rest decl-specs) `(*declaim ,@decl-specs))

(defun unproclaim (decl-spec)
  (starlisp-unproclaim decl-spec))


;;;
;;; This is so that we can capture deftype information for starlisp-proclaim to do type expansion.
;;;

(defmacro *lisp::deftype (name lambda-list &rest body)
  `(progn
     (setf (get ',name :*lisp-deftype)
       #'(lambda (&rest form)
           (destructuring-bind ,lambda-list form ,@body)))
     (cl:deftype ,name ,lambda-list ,@body)
     ))

(defmacro proclaim-*defun (function-name)
  `(eval-when (load eval compile)
     (funcall 'proclaim-*defun-1 ',function-name)))

(defun make-*defun-function (function-name)
  (let ((new-function-name (let ((*print-case* :upcase)) (format nil "*DEFUN-~A" function-name))))
    (intern new-function-name (symbol-package function-name))))

(defmacro get-*defun-function (function-name)
  `(get ,function-name '*lisp-defun))

(defmacro *trace (&rest *defun-function-names)
  `(trace ,@(mapcar 'make-*defun-function *defun-function-names))
  )

(defmacro *untrace (&rest *defun-function-names)
  `(untrace ,@(mapcar 'make-*defun-function *defun-function-names))
  )
