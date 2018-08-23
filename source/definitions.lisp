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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;   All the internal variables and data the *LISP Simulator uses are defined herein.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar *all-def-vp-sets* nil
  "A list of all the currently existing VP SETS created
   by DEF-VP-SET.  This is internal to *Lisp.
  "
  )


(defvar *all-dynamic-vp-sets* nil
  "A list of all the currently existing VP SETS created by
   CREATE-VP-SET.  This list is reset to NIL and all the
   VP SETS on it destroyed at *COLD-BOOT time.  This is
   internal to *Lisp.
  "
  )


(defvar *all-*defvar-specifications* nil
  "A list of all pvar definitions the user has defined using *DEFVAR.
   Each entry includes the pvar name, the initial value form and
   the VP SET name.  At *COLD-BOOT time the *DEFVARS are allocated space
   in the CM and their initial value forms are evaluated.  A *DEFVAR
   can be undefined using *DEALLOCATE-*DEFVARS, at which time all
   information about that *DEFVAR is removed from this list.
   This is an internal *lisp variable.
  "
  )


(defvar *all-allocate!!-pvars* nil
  "A list of all pvars allocated using ALLOCATE!!.  This list
   is reset to NIL and the pvars in it destroyed at *COLD-BOOT
   time.  This is an internal *Lisp variable.
  "
  )



(defvar *lisp-runnable-p nil
  "Whether *Lisp has been cold-booted and the user has not
   detached in the meanwhile, or loaded disgnostics 
   microcode, or done something else that renders *Lisp
   temporarily unable to access CM memory.
  "
  )


(defparameter *warn-about-obsolete-stuff* t
  "Prints out a warning message at compile time about things
   like *PSET-GRID if non-nil.
  "
  )


(defvar *cold-booting-for-first-time* t)

(defvar *do-not-print-anything-while-*cold-booting* nil)

(defvar *current-cm-configuration-vector* '#(nil nil)
  "a vector of the dimensions of the machine")
(defvar *static-grid-address-vector* '#(nil nil)
  "a global vector to hold grid address coordinates
   internally for addressing functions.")
(defvar *static-pvar-array-vector* '#(nil nil)
  "a global vector to hold pvar arrays of grid
   address pvars internally for addressing functions")

(defvar *default-grid-configuration* '(8 4) "what the simulator is initially dimensioned as")


(defvar *all-grid-addresses* nil
  "an array of arrays, indexed by cube address, which stores
   grid addresses instead of having to compute them all the time.
  "
 )

(defvar *all-cube-from-grid-addresses* nil
  "an array which is dimensioned *current-cm-configuration*, which holds
   the cube address for all possible grid addresses, so they don't have
   to be calculated all the time.
  "
 )

(defvar *dimension-subsizes* nil
  "the number of processors in each dimension and its subdimensions.  
   used by grid-from-cube-address because it needs to know how many
   processors there are in each dimension of the machine.
  "
 )


(defvar *temp-pvar-list* nil
  "list of temporary pvars not yet allocated.")

(defvar *temp-pvar-original-list* nil
  "the beginning of the orginal *temp-pvar-list*,
   used by *warm-boot to reset *temp-pvar-list*.
  "
 )



(DEFVAR *BAD-VALUE-FOR-DEALLOCATED-PVAR-ARRAY*
	'|THIS-PVAR-HAS-BEEN-DEALLOCATED.  YOU SHOULD NOT BE ACCESSING IT!|)



(DEFVAR *CONSTANT-PVAR-HASH-TABLE* nil
  "A hash table to store frequently used constant pvars instead
   of consing them up all the time.
  "
 )

(DEFVAR *MAXIMUM-NUMBER-OF-ENTRIES-IN-CONSTANT-PVAR-HASH-TABLE* 32)


(DEFVAR *CSS* NIL
  "A bit array that is 1 for each processor that
  is currently selected, and 0 for those not selected."
  )

(DEFVAR *CURRENT-CONTEXT-STACK* NIL
  "An structure containing an array of *CSS* arrays.  Used as a per vp set stack")
(DEFVAR *CSS-CURRENT-LEVEL* 1
  "The current level in the *CURRENT-CONTEXT-STACK*.")

(DEFVAR *PSET-COLLISION-ARRAY* NIL
  "Used by *PSET and *PSET-GRID to detect collisions when sending")


(DEFVAR *JUNK-TO-INITIALIZE-WITH*
  "A gensym set by cold boot to initialize temporary pvars"
 )
(DEFVAR *VALUE-TO-PRINT-WHEN-JUNK-FOUND* '*)


(DEFVAR *INSIDE-*DEFUN-P* NIL)

(DEFVAR *VALID-SCAN!!-DIRECTIONS* '(:FORWARD :BACKWARD))

(DEFVAR *SPECIAL-SCAN-FUNCTIONS*
	`((+!! +) (MAX!! MAX) (MIN!! MIN)
	  (LOGIOR!! LOGIOR) (LOGAND!! LOGAND) (LOGXOR!! LOGXOR)
	  (COPY!! ,#'(LAMBDA (X Y) (DECLARE (IGNORE Y)) X))
	  (AND!! ,#'(LAMBDA (X Y) (AND X Y)))
	  (OR!! ,#'(LAMBDA (X Y) (OR X Y)))
	  (XOR!! ,#'(LAMBDA (X Y) (OR (AND X (NOT Y)) (AND (NOT X) Y))))
	 ))


(DEFVAR *DIMENSION-KEYWORD-MAPPINGS*
	'((:X 0) (:Y 1) (:Z 2) (:X-NEWS 0) (:Y-NEWS 1) (:Z-NEWS 2)))


(DEFVAR *LEGAL-INITIALIZATION-SYMBOLS*
	'(*BEFORE-*COLD-BOOT-INITIALIZATIONS* *AFTER-*COLD-BOOT-INITIALIZATIONS*
          *BEFORE-*WARM-BOOT-INITIALIZATIONS* *AFTER-*WARM-BOOT-INITIALIZATIONS*
	 ))


(defvar *allowed-collision-modes-for-pref!!*
	'(nil :no-collisions :collisions-allowed :many-collisions)
  "Possibilities for value of :collision-mode keyword argument in pref!! and friends"
 )


(DEFVAR **LISP-SETF-SYMBOLS* '(PREF PREF-GRID) "*Lisp macros which can be SETF-ed")


(defvar *has-a-*cold-boot-been-done-p* nil)
(defun *lisp-runnable-p () *has-a-*cold-boot-been-done-p*)

(defvar *default-default-configuration* '(8 4))

(defvar *previous-*cold-boot-dimensions* nil)

(defvar *first-*cold-boot* nil)

(defconstant *lisp-interpreter-p* nil "This is not the *Lisp interpreter")
(defconstant *lisp-simulator-p* t "This is the *Lisp simulator")


