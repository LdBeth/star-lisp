;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *LISP; -*-

(in-package '*lisp)

;;;               2
;;; compute y = 3x  + 2x + 4
;;; in parallel

(defun time-me (&optional (vp-ratio 128))
  (let-vp-set (temp (create-vp-set (list (* *minimum-size-for-vp-set* vp-ratio))))
    (*with-vp-set temp
      ;; Put some random values in X in all the CM processors.
      (*let ((x (random!! (!! 1.0)))
	     (y (!! 0.0))
	     )
	(declare (type single-float-pvar x y))
	;; Make sure the *Lisp compiler is generating
	;; optimal code.
      (with-compile-time-local-property (compile-time-prop *safety* 0)
	  ;; Cause the 1000 iterations of the
	  ;; polynomial evaluation to be timed.
	  (#+*LISP-HARDWARE cm:time #+ccl cl:time
	    (dotimes (j 200)
	      (*set y (+!! (!! 4.0) (*!! x (+!! (!! 2.0) (*!! x (!! 3.0))))))
	      )))))))




#|

;;; SAMPLE TIMING SESSION

;;; Configure the machine with 128 virtual processors.
;;; using a physical machine size of 8192.

> (*cold-boot :initial-dimensions '(1024 1024))
8192
(1024 1024)
> (/ (* 1024 1024) 8192)
128

;;; Run the function.

> (time-me)

;;; CM:TIME makes sure that when it is timing code,
;;; no error checking is being done.  This is what
;;; the two messages below mean.  They can be ignored.

 Warning: Turning off Paris safety for CM:TIME.
 Warning: Turning off *Lisp safety for CM:TIME.

;;; The first time CM:TIME is called it calculates for
;;; itself how fast the CM clock is running.  This
;;; is what the message below means.  It can be ignored too.

 Calibrating CM idle timer...   Calculated CM clock speed = 6.69698 MHz

;;; Here is the result of our call to CM:TIME.  It says
;;; that the total time to execute our loop 1000 times
;;; was 8.339999 seconds, and that the Connection Machine
;;; was executing for almost all of that time, 8.263904 seconds.

Evaluation of 
(DOTIMES (J 1000) (*SET Y (+!! (!! 4.0) (*!! X (+!! (!! 2.0) (*!! X (!! 3.0))))))) 
took 8.339999 seconds of elapsed time, 
during which the CM was active for 8.263904 seconds or 99.00% of the total elapsed time.
NIL

;;; Do it again to insure that we get approimately the
;;; same results.

> (time-me)

Evaluation of 
(DOTIMES (J 1000) (*SET Y (+!! (!! 4.0) (*!! X (+!! (!! 2.0) (*!! X (!! 3.0))))))) 
took 8.259999 seconds of elapsed time, 
during which the CM was active for 8.259240 seconds or 100.00% of the total elapsed time.
NIL




;;; OK, how many floating point operations per second
;;; is the Connection Machine doing?

;;; 128   = number of virtual processors per physical processor.
;;; 8192  = number of physical processors.
;;; 1000  = number of times we executed the loop.
;;; 4     = number of multiplies (2) plus the number of additions (2)

> (* 128 8192 1000 4)
4194304000

;;; So the above number is the total number of floating point
;;; operations the Connection Machine executed.
;;; If we divide this number by the total amount of
;;; time we will determine the number of floating point
;;; instructions per second (FLOPS) the Connection Machine
;;; is performing.

> (/ * 8.259)
5.078464705170117E8

;;; So the CM is doing 500 million flops, or 500 Megaflops, or 0.5 Gigaflops.
;;; If we had a full 64K CM instead of using an 8K CM, we would multiply
;;; this number by 8, to arrive at 4.0 Gigaflops.

;;; So we can say that a full Connection Machine can evaluate a quadratic
;;; polynomial at a rate of 4.0 Gigaflops.

|#
