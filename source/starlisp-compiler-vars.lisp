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


(defvar *compilep* t)
(defvar *warning-level* :normal)
(defvar *inconsistency-action* :warn)
(defvar *safety* 3)
(defvar *slc-print-length* 4)
(defvar *slc-print-level* 3)
(defvar *optimize-bindings* 'cspeed<3)
(defvar *optimize-peephole* 'cspeed<3)
(defvar *pull-out-subexpressions* nil)
(defvar *use-always-instructions* nil)
(defvar *machine-type* 'current)
(defvar *compiling* nil)
(defvar *verbose* nil)
(defvar *generate-comments* t)
(defvar *immediate-error-if-location* t)
(defvar *space* 1)
(defvar *compilation-speed* 1)
(defvar *speed* 1)
(defvar *constant-fold* t)
(defvar *verify-type-declarations* 'current-safety)
(defvar *add-declares* nil)
