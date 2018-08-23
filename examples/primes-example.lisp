;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *LISP; -*-

(in-package '*lisp)


(defun consective-integers (n)
  (declare (type fixnum n))
  (+!! (self-address!!) (!! n))
  )


(defun print-out-primes (pvar is-prime)
  (declare (type (field-pvar 32) pvar))
  (declare (type boolean-pvar is-prime))
  (*when is-prime       
    (do-for-selected-processors (j)
      (format t "~D is a prime number~%" (pref pvar j))
      )))       


(defun is-prime (x-pvar)
  (declare (type (field-pvar 32) x-pvar))

  ;; Allocate a temporary boolean pvar
  ;; which we will return.  As we discover
  ;; non-primes in processors it will be set to NIL
  ;; in those processors.

  (*let ((prime t!!))
    (declare (type boolean-pvar prime))

    ;; What is the biggest divisor we need to test?

    (let ((max-divisor-needed (isqrt (*max x-pvar))))

      ;; Iterate from 2 up to max-divisor-needed
      
      (dotimes (j (- max-divisor-needed 1))
        (let ((divisor (+ j 2)))
          (declare (type fixnum divisor))

          ;; When the divisor divides evenly one of the numbers
          ;; set the pvar PRIME to NIL in that processor.

          (*when (and!! (zerop!! (mod!! x-pvar (!! divisor)))
                        (not!! (=!! x-pvar (!! divisor)))
                        )
            (*set prime nil!!)
            ))))

    ;; Return our result.

    prime

    ))



;;; Find all prime numbers up to N, inclusive.


(defun find-primes (n)
  (declare (type fixnum n))

  (terpri)

  ;; We start with the number 2, instead of 0 and 1.
  ;; Also figure out how many processors there are.

  (let ((start 2)
        (number-of-processors (*sum (!! 1)))
        )
    (declare (type fixnum start number-of-processors))

    ;; Loop until the numbers we are testing exceed N

    (loop
      (when (> start n) (return-from find-primes nil))

      ;; create a pvar called X-PVAR with consecutive integers
      ;; starting at START.

      (*let ((x-pvar (the (signed-pvar *) (consective-integers start))))
        (declare (type (field-pvar 32) x-pvar))

        ;; Make sure we don't test any numbers bigger than N.
        ;; Print out the primes we find by calling IS-PRIME

        (*when (<!! x-pvar (!! n))
          (print-out-primes x-pvar (is-prime x-pvar))
          ))

      ;; Increment start so that next iteration we
      ;; test the next batch of numbers.

      (incf start number-of-processors)

      )))
