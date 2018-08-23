;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *LISP; -*-

(in-package :*lisp)
						
;;;; Author:  JP Massar.

;;;; The object of this exercise is to read in a large piece of
;;;; text into the Connection Machine from the front end, determine
;;;; all contiguous non-blank sequences of characters ('words')
;;;; and create a vp set which contains one word per processor.

;;;; This example illustrates many features of Release 5.0 *Lisp,
;;;; including *defstruct, array pvars, 1-dimensional NEWS 
;;;; communication, dynamically allocated vp sets and
;;;; communication between different vp sets.

;;;; This example is to be *compiled with the *Lisp code walker
;;;; enabled.  It also demonstrates the Release 6.0 scalar promotion
;;;; feature.


;;; Set this variable to T to print out intermediate values
;;; while the computation is proceeding.

(defvar *text-processing-example-verbose* nil)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant max-word-length 32)
  (defconstant max-word-length-in-bits 5)
  (deftype part-of-speech-type () `(unsigned-byte 5))
  (deftype word-length-pvar-type ()
    `(pvar (unsigned-byte max-word-length-in-bits))
    ))

;;;; Here is the definition of the *defstruct an instance
;;;; of which will contain our results.  The part-of-speech
;;;; slot is ignored in this example.  

(*defstruct word
  (characters
    (make-array #.max-word-length :element-type 'string-char)
    :type string
    :cm-type (vector-pvar string-char #.max-word-length)
    :cm-initial-value
    (make-array!! #.max-word-length
		  :initial-element (!! #\Space)
		  :element-type 'string-char
		  ))
  (length 0 :type (unsigned-byte 6))
  (part-of-speech 0 :type part-of-speech-type)
  )


;;;; This is the definition of the vp set which will contain
;;;; an instance of the word *defstruct when we are done.
;;;; We do not define how big it is at this time.  That will
;;;; depend on how may words we find in the text we are
;;;; to process, so we will allocate an appropriate number
;;;; of processors during execution.

(def-vp-set words-vp-set nil
  :*defvars
  ((word-pvar (make-word!!) "" (pvar word)))
  )

;;;; Here is the main routine.  It takes a piece of text
;;;; in the form of a Common Lisp string.



(defun do-text-processing (string)
  (declare (type string string))
  
  (let ((string-length (length string)))
    (declare (type fixnum string-length))
    
    ;; Clear out the word array, in case we've used it before.
    
    ;; Allocate a vp set big enough to hold all
    ;; the characters in the string, one character
    ;; per processor.
    
    (let-vp-set
     (char-vp-set
      (create-vp-set
       (list (max *minimum-size-for-vp-set*
                  (next-power-of-two->= string-length)
                  ))))
     
     
     ;; Get into this newly created vp set and allocate
     ;; some temporary variables we will need.
     
     (*with-vp-set 
      char-vp-set
      
      (*let (
             text start-word-p end-word-p space-p
                  character-position-in-word word-number
                  )
            (declare
             (type string-char-pvar text)
             (type boolean-pvar start-word-p end-word-p space-p)
             (type (signed-pvar 32) character-position-in-word)
             (type (field-pvar 32) word-number)
             )
            
            ;; Move the string into the CM.  Note that this
            ;; VP Set is 1-dimensional, but we choose to look
            ;; at it in terms of a 1-dimensional NEWS grid.
            
            (progn
              ;;(with-compile-time-local-property (compile-time-prop *compilep* nil)
              (array-to-pvar-grid string text :grid-end (list string-length))
              )
            
            (when *text-processing-example-verbose*
              (pppdbg text :mode :grid :end (list string-length))
              )
            
            ;; Select those processors which contain valid characters.
            
            (*when (<!! (self-address-grid!! 0) string-length)
                   
                   ;; Figure out which characters are spaces, which
                   ;; characters begin words, and which characters
                   ;; end words.  A word is thus defined as the
                   ;; characters between a start and end bit.
                   
                   (determine-spaces-start-and-ends
                    string-length text space-p start-word-p end-word-p
                    )
                   
                   (when *text-processing-example-verbose*
                     (pppdbg space-p :mode :grid :end (list string-length))
                     (pppdbg start-word-p :mode :grid :end (list string-length))
                     (pppdbg end-word-p :mode :grid :end (list string-length))
                     )
                   
                   ;; Figure out the position in each word of each
                   ;; character, and assign a unique number to
                   ;; each word.  Each character in each word
                   ;; will know this unique number.
                   
                   (determine-char-position-and-word-number
                    space-p start-word-p
                    character-position-in-word word-number
                    )
                   
                   (when *text-processing-example-verbose*
                     (pppdbg character-position-in-word :mode :grid :end (list string-length))
                     (pppdbg word-number :mode :grid :end (list string-length))
                     )
                   
                   (let ((how-many-words (*sum (if!! start-word-p 1 0))))
                     
                     (when *text-processing-example-verbose*
                       (print (list 'how-many-words how-many-words))
                       )
                     
                     ;; Now instantiate the vp set that will hold
                     ;; the word *defstruct pvar instance with
                     ;; a number of processors big enough to handle
                     ;; the number of words that exist in the text.
                     
                     (with-processors-allocated-for-vp-set 
                         (words-vp-set :dimensions (list (max *minimum-size-for-vp-set*
                                                              (next-power-of-two->= how-many-words)
                                                              )))
                       (*with-vp-set
                        words-vp-set
                        (dotimes (j max-word-length)
                          (*setf (aref!! (word-characters!! word-pvar) (!! j)) #\Space)
                          ))
                       
                       ;; Send the characters from the characters vp set
                       ;; to the words vp set, into the appropiate array
                       ;; element of the array slot of the *defstruct pvar
                       ;; depending on that characters position within
                       ;; the word.
                       
                       (dotimes (j max-word-length)
                         (*when (=!! character-position-in-word (!! j))
                                ;; No sense doing a costly *pset if no processors are active.
                                (if (*or t!!)
                                    (*pset
                                     :no-collisions
                                     text
                                     (aref!! (word-characters!! word-pvar) (!! j))
                                     word-number
                                     :vp-set words-vp-set
                                     ))))
                       
                       ;; Now figure out how long each word is.  The
                       ;; array was initially filled with spaces, so
                       ;; the first space we find that still exists
                       ;; determines how long the word is.
                       
                       (*with-vp-set words-vp-set
                                     ;; Since we don't want to make a copy of the big array 
                                     ;; (word-characters!! word-pvar) and since position!!
                                     ;; doesn't *compile according to the 6.0 Release Notes
                                     ;; we use an alias!! to prevent a copy from being made.
                                     (*setf (word-length!! word-pvar)
                                            (position!! #\Space (alias!! (word-characters!! word-pvar))))
                                     (*when (minusp!! (word-length!! word-pvar))
                                            (*setf (word-length!! word-pvar) (the fixnum max-word-length)))
                                     )
                       
                       ;; Now print out our results in a nice format
                       
                       (dotimes (j how-many-words)
                         (let ((front-end-word (pref word-pvar j)))
                           (format t "~%Processor ~D. Length: ~D.  Word: ~A"
                             j (word-length front-end-word) (concatenate 'string (word-characters front-end-word))
                             )))
                       
                       )
                     
                     )))))))


(defun determine-spaces-start-and-ends

       (string-length text space-p start-word-p end-word-p)

  (declare (type fixnum string-length))
  (declare (type string-char-pvar text))
  (declare (type boolean-pvar space-p start-word-p end-word-p))

  (*set space-p (char=!! text #\Space))

  ;; A character begins a word if it is the first character
  ;; and it is not a space, or it is not a space and the
  ;; previous character is a space.  Since we are viewing
  ;; the machine as a 1-d grid, we can use NEWS!! to
  ;; retrieve the character value in the previous processor.
  ;; If we were viewing the machine in cube order, we would
  ;; have had to use PREF!!, which is significantly slower
  ;; than NEWS!!.

  (*set start-word-p
	(and!! (not!! space-p)
	       (or!! (zerop!! (self-address-grid!! 0))
		     (char=!! #\Space (news!! text -1))
		     )))

  ;; A character ends a word if it is the last character
  ;; and it is not a space, or it is not a space and
  ;; the next character is a space.

  (*set end-word-p
	(and!! (not!! space-p)
	       (or!! (=!! (self-address-grid!! 0)
			  (1-!! string-length))
		     (char=!! #\Space (news!! text 1))
		     ))))


(defun determine-char-position-and-word-number
       (space-p start-word-p character-position-in-word word-number)

  (declare (type boolean-pvar space-p start-word-p))
  (declare (type (signed-pvar 32) character-position-in-word))
  (declare (type (field-pvar 32) word-number))

  ;; If a character is a space, that it has no position inside
  ;; a word.  Use -1 to indicate this.  Otherwise, figure out
  ;; the character's position from the start of the word by
  ;; using scan!! to add up 1's starting at the start bit.

  (*set character-position-in-word
	(if!! space-p
	      -1
	      (1-!! (scan!! 1 '+!! :dimension 0 :segment-pvar start-word-p))
	      ))

  ;; There are as many words as there are start bits.
  ;; Enumerate these start bits, and then tell each
  ;; character in the word this unique number.

  (*when start-word-p (*set word-number (enumerate!!)))
  (*set word-number
	(scan!! word-number
		'copy!! :dimension 0 :segment-pvar start-word-p
		)))



#|

  (progn (*cold-boot) (do-text-processing "This is some text to process"))

Below is the output from running the above command with *text-processing-example-verbose* set to T.

TEXT: #\T #\h #\i #\s #\Space #\i #\s #\Space #\s #\o #\m #\e #\Space #\t #\e #\x #\t #\Space #\t #\o #\Space #\p #\r #\o #\c #\e 
#\s #\s 
SPACE-P: NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL NIL 
START-WORD-P: T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL 
END-WORD-P: NIL NIL NIL T NIL NIL T NIL NIL NIL NIL T NIL NIL NIL NIL T NIL NIL T NIL NIL NIL NIL NIL NIL NIL T 
CHARACTER-POSITION-IN-WORD: 0 1 2 3 -1 0 1 -1 0 1 2 3 -1 0 1 2 3 -1 0 1 -1 0 1 2 3 4 5 6 
WORD-NUMBER: 0 0 0 0 0 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 5 5 5 5 5 5 5 
(HOW-MANY-WORDS 6) 
Processor 0. Length: 4.  Word: This                            
Processor 1. Length: 2.  Word: is                              
Processor 2. Length: 4.  Word: some                            
Processor 3. Length: 4.  Word: text                            
Processor 4. Length: 2.  Word: to                              
Processor 5. Length: 7.  Word: process                         
NIL
  
|#
