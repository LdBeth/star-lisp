;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: *SIM-I; Muser: yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defun dimension-size (dimension)
  "returns the size of the specified dimension"
  (check-dimension dimension *current-vp-set* 'dimension-size)
  (incf-use-count 'dimension-size)
  (nth dimension *current-cm-configuration*)
  )

(defun dimension-address-length (dimension)
  "Returns the number of bits necessary to hold an address along the specified dimension"
  (check-dimension dimension *current-vp-set* 'dimension-address-length)
  (incf-use-count 'dimension-address-length)
  (nth dimension (vp-set-grid-address-lengths *current-vp-set*))
  )


(defun self-address-grid!! (dimension-pvar)
  (simple-pvar-argument!! dimension-pvar)
  (incf-use-count 'self-address-grid!!)
  (safety-check (check-dimension-pvar dimension-pvar *current-vp-set* 'self-address-grid!!))
  (internal-grid-from-vp-cube-address!! (self-address!!) dimension-pvar *current-vp-set* 'self-address-grid!!)
  )


(defun cube-from-grid-address (&rest indices)
  (incf-use-count 'cube-from-grid-address)
  (internal-cube-from-vp-grid-address *current-vp-set* indices 'cube-from-grid-address)
  )

(defun cube-from-vp-grid-address (vp-set &rest indices)
  (incf-use-count 'cube-from-vp-grid-address)
  (internal-cube-from-vp-grid-address vp-set indices 'cube-from-vp-grid-address)
  )

(defun grid-from-cube-address (cube-address dimension)
  (incf-use-count 'grid-from-cube-address)
  (internal-grid-from-vp-cube-address *current-vp-set* cube-address dimension 'grid-from-cube-address)
  )

(defun grid-from-vp-cube-address (vp-set cube-address dimension)
  (incf-use-count 'grid-from-vp-cube-address)
  (internal-grid-from-vp-cube-address vp-set cube-address dimension 'grid-from-vp-cube-address)
  )


(defun cube-from-grid-address!! (&rest index-pvars)
  (incf-use-count 'cube-from-grid-address!!)
  (internal-cube-from-vp-grid-address!! 'cube-from-grid-address!! t nil *current-vp-set* index-pvars)
  )

(defun cube-from-vp-grid-address!! (vp-set &rest index-pvars)
  (incf-use-count 'cube-from-vp-grid-address!!)
  (internal-cube-from-vp-grid-address!! 'cube-from-grid-address!! t nil vp-set index-pvars)
  )


(defun grid-from-cube-address!! (cube-address-pvar dimension-pvar)
  (incf-use-count 'grid-from-cube-address!!)
  (internal-grid-from-vp-cube-address!! cube-address-pvar dimension-pvar *current-vp-set* 'grid-from-cube-address!!)
  )

(defun grid-from-vp-cube-address!! (vp-set cube-address-pvar dimension-pvar)
  (incf-use-count 'grid-from-vp-cube-address!!)
  (internal-grid-from-vp-cube-address!! cube-address-pvar dimension-pvar vp-set 'grid-from-vp-cube-address!!)
  )


(defun internal-grid-from-vp-cube-address!! (cube-address-pvar dimension-pvar vp-set function-name)
  (simple-pvar-argument!! cube-address-pvar dimension-pvar)
  (safety-check
    (progn
      (vp-set-check vp-set function-name)
      (new-two-pvar-check cube-address-pvar dimension-pvar function-name)
      (check-cube-address-pvar cube-address-pvar vp-set function-name)
      (check-dimension-pvar dimension-pvar vp-set function-name)
      ))
  (let* ((return-pvar (allocate-temp-general-pvar))
	 (array-of-grid-addresses (vp-set-array-of-grid-addresses vp-set))
	 )
    #-KCL
    (declare (type (array fixnum 2) array-of-grid-addresses))
    (when
      (with-selected-general-pvar-arrays
	(processor) (return-array cube-address-array dimension-array) (return-pvar cube-address-pvar dimension-pvar)
	(setf (aref return-array processor)
	      (aref array-of-grid-addresses
		    (the fixnum (aref cube-address-array processor))
		    (the fixnum (aref dimension-array processor))
		    )))
      (make-non-void return-pvar)
      )
    return-pvar
    ))

(defun off-grid-border-relative-p!! (&rest relative-addresses)
  (simple-pvar-argument!! &rest relative-addresses)
  (incf-use-count 'off-grid-border-relative-p!!)
  (not!!
    (internal-cube-from-vp-grid-address!!
      'off-grid-border-relative-p!! nil t *current-vp-set* relative-addresses
     )))

(defun off-grid-border-p!! (&rest grid-addresses)
  (simple-pvar-argument!! &rest grid-addresses)
  (incf-use-count 'off-grid-border-p!!)
  (not!!
    (internal-cube-from-vp-grid-address!!
      'off-grid-border-p!! nil nil *current-vp-set* grid-addresses
     )))

(defun off-vp-grid-border-p!! (vp-set &rest grid-addresses)
  (simple-pvar-argument!! &rest grid-addresses)
  (incf-use-count 'off-vp-grid-border-p!!)
  (not!!
    (internal-cube-from-vp-grid-address!!
      'off-grid-border-p!! nil nil vp-set grid-addresses
     )))





