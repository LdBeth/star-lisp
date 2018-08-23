;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *LISP; -*-

(in-package '*lisp)
						
(defun run-examples ()

  (*warm-boot)

  (format t 
	  "~%~%***** Running text-processing example.  Output should be: ~%~@
	       Processor 0. Length: 4.  Word: This ~@
	       Processor 1. Length: 2.  Word: is ~@
	       Processor 2. Length: 4.  Word: some ~@
	       Processor 3. Length: 4.  Word: text ~@
	       Processor 4. Length: 2.  Word: to ~@
	       Processor 5. Length: 7.  Word: process ~@
	  "
	  )
  (format t "~%***** Output is:~%~%")
  (do-text-processing "This is some text to process")

  (format t "~%~%***** Running very long add.  Result should be bitwise addition of arguments~%~%")
  (test-very-long-add)

  #+*LISP-HARDWARE
  (format t "~%~%***** Running canon matrix multiply.  Output should be 5x5 grid of 128.0's~%")
  #+*LISP-SIMULATOR
  (format t "~%~%***** Running canon matrix multiply.  Output should be 5x5 grid of 8.0's~%")
  (example-canon)

  (format t 
	  "~%~%**** Running combinations example.  Output should be: ~%~@
            ITEM!!: #\\A #\\B #\\C #\\D #\\E #\\F ~@
	    FIRST-RESULT!!: #\\A #\\A #\\A #\\A #\\A #\\B #\\B #\\B #\\B #\\C #\\C #\\C #\\D #\\D #\\E ~@
	    SECOND-RESULT!!: #\\B #\\C #\\D #\\E #\\F #\\C #\\D #\\E #\\F #\\D #\\E #\\F #\\E #\\F #\\F ~@
	  "
	  )
  (format t "~%***** Output is:~%~%")
  (test-pair-combinations)
  
  (format t "~%~%***** running bitblt example.  Output should be three random points")
  (initialize-points)
  (assign-points-to-processors-randomly :pretty-print t)

  (format t "~%~%***** Running primes example.  Output should be: ~%~@
             2 is a prime number~@
             3 is a prime number~@
             5 is a prime number~@
             7 is a prime number~@
            "
	  )
  (format t "~%***** Output is:~%~%")
  (find-primes 10)

  #-*LISP-SIMULATOR
  (progn
    (format t "~%~%***** Running sample timing of quadratic polynomial twice.~%")
    (let ((*interpreter-safety* 0) (*immediate-error-if-location* nil))
      (cm:set-safety-mode 0)
      (time-me)
      (time-me)
      (cm:set-safety-mode 1)
      ))

  (format t "~%~%***** End execution of examples")

  )
