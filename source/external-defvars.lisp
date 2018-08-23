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


(defvar *before-*cold-boot-initializations*  nil "these get evaled before each *Cold-Boot")
(defvar *before-*warm-boot-initializations*  nil "These get evaled before each *Warm-Boot")

(defvar *after-*cold-boot-initializations*  nil "These get evaled after each *Cold-Boot")
(defvar *after-*warm-boot-initializations*  nil "These get evaled after each *Warm-Boot")

(declaim (special t!! nil!!))

(defvar t!!)
(defvar nil!!)


(defvar *interpreter-safety* 3
  "Control the level of error checking when the Starlisp interpreter is executing.
   Valid values are 0,1,2 or 3.
  "
  )


(defvar *current-vp-set* nil
  "Dynamically bound variable bound by *with-vp-set and by *cold-boot.
   It defines Starlisp's currently active VP SET.
  "
  )


(defvar *default-vp-set* nil
  "Defined at *cold-boot time.  This is a VP SET which is by default
   2 dimensional and has as many virtual processors as there
   are physical processors in the machine.  If the user specifies
   an :initial-dimensions argument to *cold-boot, then *default-vp-set*
   will take on the dimensionality so specified.
  "
  )


(defvar *minimum-size-for-vp-set* nil
	"The smallest number of virtual processors a VP SET may be defined with.
         It is an error to access the value of this parameter before the
         first *COLD-BOOT is done.
        "
	)


(defvar *number-of-processors-limit* nil
  "The number of virtual processors defined by the current 
   vp set.  Before the first *COLD-BOOT it is an error
   to access this parameter.
  "
  )


(defvar *log-number-of-processors-limit* nil
  "Assuming a power of two sized VP SET, the log base 2
   of *number-of-processors-limit*.  If *number-of-processors-limit*
   is not a power of two, then this parameter has the
   value (integer-length *number-of-processors-limit*).
   This is useful in determining the number of bits needed
   to hold the address of a virtual processor within
   the current VP SET.  Before the first *COLD-BOOT it is an
   error to access this parameter.
  "
  )


(defvar *current-send-address-length* nil
  "The same as *log-number-of-processors-limit*.
   *log-number-of-processors-limit* should become obsolete.
  "
  )


(defvar *current-cm-configuration* nil
  "The dimensions of the current vp set, as a list.  Before
  the first *COLD-BOOT it is an error to access this
  parameter.
  "
  )


(defvar *current-grid-address-lengths* nil
  "A list, the jth element being the number of bits necessary
   to hold a grid address component for the jth dimension
   of the current VP SET.  Before the first *COLD-BOOT it
   is an error to access this parameter.
  "
  )


(defvar *number-of-dimensions* nil
  "The number of dimensions specified by the current vp set.
   Before the first *COLD-BOOT it is an error to access
   this parameter.
   "
  )

(defparameter *array-rank-limit 8 "The maximum portable number of dimensions allowed")
(defparameter *array-dimension-limit #.(expt 2 15) "The maximum portable size of any one dimension")
(defparameter *array-total-size-limit #.(expt 2 15) "The maximum portable total size in elements of an array")

(defvar *ppp-default-mode* :cube "Default value for keyword argument :mode to PPP")
(defvar *ppp-default-format* "~S " "Default value for keyword argument :format to PPP")
(defvar *ppp-default-per-line* nil "Default value for keyword argument :per-line to PPP")
(defvar *ppp-default-title* nil "Default value for keyword argument :title to PPP")
(defvar *ppp-default-start* 0 "Default value for keyword argument :start to PPP")
(defvar *ppp-default-end* *number-of-processors-limit*
  "Default value for keyword argument :end to PPP")
(defvar *ppp-default-ordering* nil)
(defvar *ppp-default-processor-list* nil)


(defparameter *char-code-limit nil "Limit of valid character codes")
(defparameter *char-code-length nil "Length in bits of the code subfield of a character pvar")
(defparameter *character-length nil "Length in bits of a character pvar")

(defvar *starlisp-simulator-features-symbol*
	(intern (concatenate 'string "*" "LISP-SIMULATOR"))
  "The symbol to be pushed onto *features* if this is the Simulator"
  )

(defvar *starlisp-hardware-features-symbol*
	(intern (concatenate 'string "*" "LISP-HARDWARE"))
  "The symbol to be pushed onto *features* if this is the Interpreter/Compiler"
  )
