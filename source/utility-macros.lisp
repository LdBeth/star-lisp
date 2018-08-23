;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: *SIM-I -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;
;;; Common Lisp doesn't have comment!
;;;


(defmacro comment (&rest foo) (declare (ignore foo)) ''comment)

(defmacro pvar-check (x) `(assert (pvar-p ,x)))

(defmacro check-pvar-arglist (arglist) `(dolist (j ,arglist) (pvar-check j)))


(defmacro safety-check (&body body)
  `(when (plusp *interpreter-safety*) ,@body)
  )

;;;; ****************************************************************************
;;;;
;;;;                         MISCELLANEOUS MACROS
;;;;
;;;; ****************************************************************************


(defmacro valid-integer-range (x lower-inclusive-limit upper-inclusive-limit)
  `(and (integerp ,x) (>= ,x ,lower-inclusive-limit) (<= ,x ,upper-inclusive-limit))
 )

(defmacro valid-integer-range-exclusive (x lower-limit upper-limit)
  `(and (integerp ,x) (>= ,x ,lower-limit) (< ,x ,upper-limit))
 )

(defmacro valid-integer-range-inclusive (x lower-limit upper-limit)
  `(and (integerp ,x) (>= ,x ,lower-limit) (< x ,upper-limit))
 )

(defmacro valid-start-and-end (start end &key (min-start 0) max-end)
  `(and (integerp ,start) (integerp ,end)
	(>= ,start ,min-start) ,@(if max-end `((< ,end ,max-end)) nil)
	(<= ,start ,end)
    ))

(defmacro valid-start-and-limit (start limit &key (min-start 0) max-limit)
  `(and (integerp ,start)
	(integerp ,limit)
	(>= ,start ,min-start)
	,@(if max-limit `((<= ,limit ,max-limit)) nil)
	(< ,start ,limit)
    ))

(defmacro while (condition &rest body)
   `(loop (if (null ,condition) (return)) ,@body))

(defmacro until (condition &rest body)
   `(loop (if ,condition (return)) ,@body))

(defmacro for++ ((var init end) &rest body)
  `(let ((,var ,init))
     (loop
       (if (<= ,var ,end)
           (progn (progn ,@body) (incf ,var))
           (return)
        ))))

(defmacro for-- ((var init end) &rest body)
  `(let ((,var ,init))
     (loop
       (if (>= ,var ,end)
           (progn (progn ,@body) (decf ,var))
           (return)
        ))))


(defmacro for (((var init &optional result) endtest step) &rest body)
  `(let ((,var ,init))
     (loop
       (and ,endtest (return ,result))
       (progn ,.body)
       ,step
      )))

