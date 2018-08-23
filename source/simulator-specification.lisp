;;; -*- Mode: LISP;  Syntax: COMMON-LISP; Package: (*SIM-I COMMON-LISP-GLOBAL); Base: 10; Muser: yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;; A FEW MISCELLANEOUS ITEMS


;;;; To conditionalize code, we put the symbols *LISP-SIMULATOR or *LISP-HARDWARE
;;;; onto the *FEATURES* list.  Unfortunately, various implementations have
;;;; decided to do things a bit differently, some using keywords and some not.
;;;; This defines a portable mechanism for adding new features.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun proper-symbol-for-*features* (x)
    (intern (symbol-name x) (find-package "KEYWORD"))
   )
  (defun add-new-feature-to-*features* (x)
    (pushnew (proper-symbol-for-*features* x) *features*)
   ))


(eval-when (:compile-toplevel)
  (when (member (proper-symbol-for-*features* *starlisp-hardware-features-symbol*) *features*)
    (error "~S is on the features list.  You must take it off ~@
            while compiling the Simulator."
	   *starlisp-hardware-features-symbol*
	   )))

 

;;;; Certain exportable functions and variables are specific to the Simulator.
;;;; They are defined and exported here.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *simulator-exported-functions*
  '(RESET-*LISP-FUNCTION-USE-STATISTICS DISPLAY-*LISP-FUNCTION-USE-STATISTICS)
  ))

;; JP 10/23/02  Got rid of import and export at top level --
;; was causing Allegro compiler warning.

;; (import *simulator-exported-functions* :*SIM)
;; (export *simulator-exported-functions* :*SIM)

;; 11/01/02  Corman doesn't like symbols in the IMPORT list.
;; 03/09/03  CLISP doesn't like using DEFPACKAGE twice.
;;           It destroys nicknames and use lists unless provided again.
;; Screw it for now.
;; 04/13/03  Lispworks destroys nicknames too.

#-(OR :ccl :CORMANLISP :CLISP :LISPWORKS)
(cl-user::defpackage-with-lists-symbols "*SIM"
    (:import-from-package "*SIM-I"
  :import-from-list-symbol *simulator-exported-functions*
  :export-list-symbol *simulator-exported-functions*
  ))


;;;; Put *LISP-SIMULATOR on *FEATURES* list so users can conditionize
;;;; code between hardware and simulator.


(add-new-feature-to-*features* *starlisp-simulator-features-symbol*)
(add-new-feature-to-*features* '*LISP-SIMULATOR-f20)

(defvar *starlisp-simulator-header* "Thinking Machines Starlisp Simulator")
(defparameter *starlisp-simulator-version* 20.0)
(defparameter *expiration-date* '(2020 12 31))

(setq *minimum-size-for-vp-set* 1)


