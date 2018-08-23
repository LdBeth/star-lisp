;;; -*- MODE: LISP; SYNTAX: COMMON-LISP; PACKAGE: *SIM-I; BASE: 10; MUSER: YES; -*-

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


(defun intern-in-starlisp-package (string)
  (intern string (find-package *starlisp-package-name*)))

(defun intern-in-starlisp-internal-package (string)
  (intern string (find-package *starlisp-internal-package-name*))
  )

(defun unintern-if-wrong (list-of-symbols package)
  (if (not (packagep package)) (setq package (find-package package)))
  (dolist (symbol list-of-symbols)
    (let ((other-symbol (find-symbol (symbol-name symbol) package)))
      (when (and other-symbol 
                 (not (eq other-symbol symbol))
                 (not (member other-symbol (package-shadowing-symbols package))))
        (warn "Uninterning ~A::~A because it is ugly." (package-name package) other-symbol)
        (unintern other-symbol package)))))


(defun import-then-export (import-into-package export-from-package list-of-symbols)
  (unintern-if-wrong list-of-symbols import-into-package)
  (import list-of-symbols import-into-package)
  (dolist (package (package-used-by-list (find-package export-from-package)))
    (unintern-if-wrong list-of-symbols package))
  (export list-of-symbols export-from-package)
  )


(defvar *all-external-symbols* nil
  "All of Starlisp's defined function, macros, *defuns, symbols and whatnot"
  )


(defparameter *legal-def-starlisp-type-specifiers*
	      '(
		:function :macro :*defun :variable :compiler-variable :declaration :deftype :other
		function macro variable compiler-variable declaration deftype other
		))


(defparameter  *legal-def-starlisp-info-characters* "ONCDSMEVAB")


(defparameter *character-to-english-mapping* 
  '((O "is obsolete as of Release 5.0.")
    (N "is new with Release 5.0.")
    (C "has been augmented to handle complex numbers for Release 5.0.")
    (D "has been augmented to handle 'N' dimensions for Release 5.0.")
    (S "has had its syntax changed for Release 5.0.")
    (M "has had its definition/meaning/effects changed for Release 5.0.")
    (E "is experimental.  It may go away in a future Starlisp release.")
    (V "has beem augmented to deal with inter-vp-set communication for Release 5.0.")
    (A "is new with Release 5.1.")
    (B "is new with Release 5.2.")
    ))


(defparameter *doc-code-to-english-mapping*
  '((N5 "Starlisp Release Notes, Version 5.0")
    (M5 "Starlisp Reference Manual, Version 5.0")
    (S5 "Starlisp Reference Manual Supplement, Version 5.0")
    (CG "Starlisp Compiler Guide, Version 5.0")
    ))


; :docptrs
;
; N5	Starlisp Release Notes, Version 5.0
; M5	Starlisp Reference Manual, Version 5.0
; S5	Starlisp Reference Manual Supplement, Version 5.0
; CG	Starlisp Compiler Guide, Verison 5.0
;
; sometimes these are followed by one or more page numbers, as in: "M5,12,49"
; also sometimes more than one document is referenced, as in: "M5,33;S5"
; -Lauranne
;


(defparameter *load-starlisp-interface-info* t
  "Whether all the information about starlisp symbols will be stored at run time"
  )


(defun set-starlisp-info-for-starlisp-symbol (symbol type info-string)
  (when *load-starlisp-interface-info*
    (setf (get symbol :starlisp-type) type)
    (setf (get symbol :starlisp-descriptor) info-string)
    )
  (pushnew symbol *all-external-symbols*)
  )


(defmacro def-starlisp (name type &key arglist docptr info &allow-other-keys)

  (assert (stringp name) () "The name argument for def-starlisp must be a string!!")
  (setq name (string-upcase name))

  (assert (member type *legal-def-starlisp-type-specifiers* :test #'eq) ()
	  "The type argument for def-starlisp can only be one of ~S"
	  *legal-def-starlisp-type-specifiers*
	  )

  (assert (or (null arglist) (stringp arglist)) ()
	  "The :arglist argument for def-starlisp must be a string"
	  )
  (when (null arglist) (setq arglist ""))

  (when (not *load-starlisp-interface-info*)
    (return-from def-starlisp
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (set-starlisp-info-for-starlisp-symbol (intern-in-starlisp-package ,name) nil nil)
	 )))

  (assert (or (null docptr) (stringp docptr)))
  (let ((info-characters "") (keyword-type type))
    (when (not (keywordp keyword-type))
      (setq keyword-type (intern (symbol-name keyword-type) (find-package 'keyword)))
      )
    (when info
      (assert (or (keywordp info) (stringp info)) ()
	      "The info argument for def-starlisp must be a keyword composed ~
               of all the desired characters or a similar string"
	      )
      (let ((characters (if (stringp info) info (symbol-name info))))
	(dotimes (j (length characters))
	  (let ((char (aref characters j)))
	    (assert (find char *legal-def-starlisp-info-characters* :test #'char-equal) ()
		    "The character specifier '~A' is not legal as a component of the info argument to def-starlisp.~@
                      Legal characters consist of '~A'"
		    char
		    *legal-def-starlisp-info-characters*
		    )))
	(setq info-characters (string-upcase characters))
	))
    (let ((info-string
	    (format nil "~S" `(:name ,(intern-in-starlisp-package name) :type ,keyword-type :arglist ,arglist :docptr ,docptr :info ,info-characters))
	    ))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (set-starlisp-info-for-starlisp-symbol (intern-in-starlisp-package ,name) ,keyword-type ,info-string)
	 )
      )))

