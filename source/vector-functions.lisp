;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Muser: Yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.

;;;> Bugs, comments and revisions due to porting can be sent to:
;;;> bug-starlisp@think.com.  Other than to Thinking Machines'
;;;> customers, no promise of support is intended or implied.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defun new-vector-pvar-check (pvar function)
  (new-pvar-check pvar function)
  (assert (vector-pvar-p pvar) () "~S: The argument, ~S, is not a vector pvar" function pvar)
  )

(defun vector-lengths-all-same-check (&rest vector-pvars)
  (if (zerop (length vector-pvars))
      t
      (let ((length (array-pvar-total-size (car vector-pvars))))
	(every #'(lambda (x) (= length (array-pvar-total-size x))) (cdr vector-pvars))
	)))

(*defun *vset-components (vector-pvar &rest component-pvars)
  (simple-pvar-argument!! &rest component-pvars) ;;; can't modify a temporary
  (let ((length (length component-pvars)))

    (safety-check
      (new-vector-pvar-check vector-pvar '*vset-components)
      (new-multiple-pvar-check component-pvars '*vset-components)
      (assert (or (eql length 1) (eql length (array-pvar-total-size vector-pvar))) ()
	      "~S: The number of components, ~D, does not agree with the length of the vector pvar, ~D"
	      '*vset-components length (array-pvar-total-size vector-pvar)
	      ))

    (let ((count 0))
      (*map
	#'(lambda (x) (*set x (if (eql 1 length) (car component-pvars) (nth count component-pvars))) (incf count))
	vector-pvar
	))

    ))


(defun v+-*-args-check (function-name vector-pvar more-vector-pvars)
  (safety-check
    (new-vector-pvar-check vector-pvar function-name)
    (mapcar #'(lambda (pvar) (new-vector-pvar-check pvar function-name)) more-vector-pvars)
    (apply #'vector-lengths-all-same-check vector-pvar more-vector-pvars)
    ))


(defun v+!! (vector-pvar &rest more-vector-pvars)
  (simple-pvar-argument!! vector-pvar &rest more-vector-pvars)
  (v+-*-args-check 'v+!! vector-pvar more-vector-pvars)
  (*let ((dest-pvar vector-pvar))
    (apply
      '*sim::*defun-*map
      #'(lambda (dest &rest sources)
	  (*set dest (apply #'+!! dest sources))
	  )
      dest-pvar
      more-vector-pvars
      )
    dest-pvar
    ))


(defun v-!! (vector-pvar &rest more-vector-pvars)
  (simple-pvar-argument!! vector-pvar &rest more-vector-pvars)
  (v+-*-args-check 'v-!! vector-pvar more-vector-pvars)
  (*let ((dest-pvar vector-pvar))
    (apply
      '*sim::*defun-*map
      #'(lambda (dest &rest sources)
	  (if sources
	      (*set dest (+!! dest (-!! (apply '+!! sources))))
	      (*set dest (-!! dest))
	      ))
      dest-pvar
      more-vector-pvars
      )
    dest-pvar
    ))


(defun v*!! (vector-pvar &rest more-vector-pvars)
  (simple-pvar-argument!! vector-pvar &rest more-vector-pvars)
  (v+-*-args-check 'v*!! vector-pvar more-vector-pvars)
  (*let ((dest-pvar vector-pvar))
    (apply
      '*sim::*defun-*map
      #'(lambda (dest &rest sources)
	  (*set dest (apply #'*!! dest sources))
	  )
      dest-pvar
      more-vector-pvars
      )
    dest-pvar
    ))

(defun v/!! (vector-pvar &rest more-vector-pvars)
  (simple-pvar-argument!! vector-pvar &rest more-vector-pvars)
  (v+-*-args-check 'v/!! vector-pvar more-vector-pvars)
  (*let ((dest-pvar vector-pvar))
    (apply
      '*sim::*defun-*map
      #'(lambda (dest &rest sources)
	  (*set dest (apply #'/!! dest sources))
	  )
      dest-pvar
      more-vector-pvars
      )
    dest-pvar
    ))

 
(defun dot-product!! (vector-pvar1 vector-pvar2)
  (simple-pvar-argument!! (vector-pvar1 vector-pvar2))
  (funcall 'v+-*-args-check 'dot-product!! vector-pvar1 (list vector-pvar2))
  (*let ((result (!! 0)))
        (let* ((result-pvar-array (pvar-array result))
               (vector-pvar1-displaced-array (pvar-array-displaced-array vector-pvar1))
               (vector-pvar2-displaced-array (pvar-array-displaced-array vector-pvar2))
               (vector-length (length vector-pvar2-displaced-array))
               (void t)
               )
          (with-simple-vectors (result-pvar-array vector-pvar1-displaced-array vector-pvar2-displaced-array)
            (dotimes (element vector-length)
              (let* ((element-pvar-1 (aref vector-pvar1-displaced-array element))
                     (element-pvar-2 (aref vector-pvar2-displaced-array element))
                     (element-array-1 (pvar-array element-pvar-1))
                     (element-array-2 (pvar-array element-pvar-2))
                     )
                (do-for-selected-processors-internal (processor)
                  (setq void nil)
                  (incf (aref result-pvar-array processor)
                        (* (aref element-array-1 processor) (aref element-array-2 processor))
                        )))))
          (when (not void) (make-non-void result))
          result
          )))


(defun cross-product!! (vector-pvar1 vector-pvar2)
  (simple-pvar-argument!! vector-pvar1 vector-pvar2)
  (funcall 'v+-*-args-check 'cross-product!! vector-pvar1 (list vector-pvar2))
  (assert (eql 3 (*array-total-size vector-pvar1)) () "Cross-product!! only works in 3 dimensions")
  (*let ((result vector-pvar1))
        (let* ((result-pvar-displaced-array (pvar-array-displaced-array result))
               (vector-pvar1-displaced-array (pvar-array-displaced-array vector-pvar1))
               (vector-pvar2-displaced-array (pvar-array-displaced-array vector-pvar2))
               )
          (with-simple-vectors 
              (result-pvar-displaced-array vector-pvar1-displaced-array vector-pvar2-displaced-array)
            (if (void-pvar-p result)
                result
              (let ((x1a (pvar-array (aref vector-pvar1-displaced-array 0)))
                    (y1a (pvar-array (aref vector-pvar1-displaced-array 1)))
                    (z1a (pvar-array (aref vector-pvar1-displaced-array 2)))
                    (x2a (pvar-array (aref vector-pvar2-displaced-array 0)))
                    (y2a (pvar-array (aref vector-pvar2-displaced-array 1)))
                    (z2a (pvar-array (aref vector-pvar2-displaced-array 2)))
                    (xdest (pvar-array (aref result-pvar-displaced-array 0)))
                    (ydest (pvar-array (aref result-pvar-displaced-array 1)))
                    (zdest (pvar-array (aref result-pvar-displaced-array 2)))
                    )
                (with-simple-vectors (xdest ydest zdest x1a y1a z1a x2a y2a z2a)
                  (do-for-selected-processors-internal (processor)
                    (let ((x1 (aref x1a processor))
                          (y1 (aref y1a processor))
                          (z1 (aref z1a processor))
                          (x2 (aref x2a processor))
                          (y2 (aref y2a processor))
                          (z2 (aref z2a processor))
                          )
                      (setf (aref xdest processor) (- (* y1 z2) (* z1 y2)))
                      (setf (aref ydest processor) (- (* z1 x2) (* x1 z2)))
                      (setf (aref zdest processor) (- (* x1 y2) (* y1 x2)))
                      )))
                result
                ))))))


(defun vabs-squared!! (vector-pvar) (dot-product!! vector-pvar vector-pvar))
(defun vabs!! (vector-pvar) (sqrt!! (vabs-squared!! vector-pvar)))

(defun vscale!! (vector-pvar scalar-pvar)
  (simple-pvar-argument!! vector-pvar scalar-pvar)
  (safety-check
    (new-vector-pvar-check vector-pvar 'vscale!!)
    (new-pvar-check scalar-pvar 'vscale!!)
    )
  (*let ((dest-pvar vector-pvar))
    (funcall
      '*sim::*defun-*map
      #'(lambda (dest)
	  (*set dest (*!! dest scalar-pvar))
	  )
      dest-pvar
      )
    dest-pvar
    ))


(defun vscale-to-unit-vector!! (vector-pvar)
  (vscale!! vector-pvar (/!! (vabs!! vector-pvar)))
  )


(*defun *sf-vset-components (vector-pvar &rest component-pvars)
  (*apply '*vset-components vector-pvar component-pvars)
  )


(defun general-sf-constant!! (function-name function vector-pvar scalar-pvar)
  (safety-check
    (new-vector-pvar-check vector-pvar function-name)
    (new-pvar-check scalar-pvar function-name)
    )
  (*let ((dest-pvar vector-pvar))
    (funcall
      '*sim::*defun-*map
      #'(lambda (dest)
	  (*set dest (funcall function dest scalar-pvar))
	  )
      dest-pvar
      )
    dest-pvar
    ))

(defun sf-v+-constant!! (vector-pvar scalar-pvar)
  (general-sf-constant!! 'sf-v+-constant!! '+!! vector-pvar scalar-pvar)
  )

(defun sf-v--constant!! (vector-pvar scalar-pvar)
  (general-sf-constant!! 'sf-v--constant!! '-!! vector-pvar scalar-pvar)
  )
(defun sf-v*-constant!! (vector-pvar scalar-pvar)
  (general-sf-constant!! 'sf-v*-constant!! '*!! vector-pvar scalar-pvar)
  )

(defun sf-v/-constant!! (vector-pvar scalar-pvar)
  (general-sf-constant!! 'sf-v/-constant!! '/!! vector-pvar scalar-pvar)
  )



(*defun dsf-v+-constant!! (vdest vector-pvar scalar-pvar)
  (*set vdest (general-sf-constant!! 'sf-v+-constant!! '+!! vector-pvar scalar-pvar))
  vdest
  )

(*defun dsf-v--constant!! (vdest vector-pvar scalar-pvar)
  (*set vdest (general-sf-constant!! 'sf-v--constant!! '-!! vector-pvar scalar-pvar))
  vdest
  )

(*defun dsf-v*-constant!! (vdest vector-pvar scalar-pvar)
  (*set vdest (general-sf-constant!! 'sf-v*-constant!! '*!! vector-pvar scalar-pvar))
  vdest
  )

(*defun dsf-v/-constant!! (vdest vector-pvar scalar-pvar)
  (*set vdest (general-sf-constant!! 'sf-v/-constant!! '/!! vector-pvar scalar-pvar))
  vdest
  )

(defun sf-v+!! (vector-pvar &rest more-vector-pvars) (apply 'v+!! vector-pvar more-vector-pvars))
(defun sf-v-!! (vector-pvar &rest more-vector-pvars) (apply 'v-!! vector-pvar more-vector-pvars))
(defun sf-v*!! (vector-pvar &rest more-vector-pvars) (apply 'v*!! vector-pvar more-vector-pvars))

(*defun dsf-v+!! (vdest vector-pvar &rest more-vector-pvars)
  (*set vdest (apply 'sf-v+!! vector-pvar more-vector-pvars))
  vdest
  )

(*defun dsf-v-!! (vdest vector-pvar &rest more-vector-pvars)
  (*set vdest (apply 'sf-v-!! vector-pvar more-vector-pvars))
  vdest
  )

(*defun dsf-v*!! (vdest vector-pvar &rest more-vector-pvars)
  (*set vdest (apply 'sf-v*!! vector-pvar more-vector-pvars))
  vdest
  )

(defun sf-vabs!! (vector-pvar) (vabs!! vector-pvar))
(defun sf-vabs-squared!! (vector-pvar) (vabs-squared!! vector-pvar))
(defun sf-dot-product!! (vector-pvar1 vector-pvar2) (dot-product!! vector-pvar1 vector-pvar2))
(defun sf-vscale!! (vector-pvar scalar-pvar) (vscale!! vector-pvar scalar-pvar))
(defun sf-vscale-to-unit-vector!! (vector-pvar) (vscale-to-unit-vector!! vector-pvar))
(defun sf-cross-product!! (vector-pvar1 vector-pvar2) (cross-product!! vector-pvar1 vector-pvar2))
(defun sf-vector-normal!! (vector-pvar1 vector-pvar2)
  (sf-vscale-to-unit-vector!! (sf-cross-product!! vector-pvar1 vector-pvar2))
  )

(*defun dsf-vscale!! (vdest vector-pvar scalar-pvar)
  (*set vdest (sf-vscale!! vector-pvar scalar-pvar))
  vdest
  )

(*defun dsf-vscale-to-unit-vector!! (vdest vector-pvar)
  (*set vdest (sf-vscale-to-unit-vector!! vector-pvar))
  vdest
  )

(*defun dsf-cross-product!! (vdest vector-pvar1 vector-pvar2)
  (*set vdest (cross-product!! vector-pvar1 vector-pvar2))
  vdest
  )

(*defun dsf-vector-normal!! (vdest vector-pvar1 vector-pvar2)
  (*set vdest (sf-vector-normal!! vector-pvar1 vector-pvar2))
  vdest
  )

(defun v+scalar!! (vector-pvar scalar-pvar)
  (amap!! #'(lambda (vector-element-pvar) (+!! vector-element-pvar scalar-pvar))
	  vector-pvar))

(defun v-scalar!! (vector-pvar scalar-pvar)
  (amap!! #'(lambda (vector-element-pvar) (-!! vector-element-pvar scalar-pvar))
	  vector-pvar))

(defun v*scalar!! (vector-pvar scalar-pvar)
  (amap!! #'(lambda (vector-element-pvar) (*!! vector-element-pvar scalar-pvar))
	  vector-pvar))

(defun v/scalar!! (vector-pvar scalar-pvar)
  (amap!! #'(lambda (vector-element-pvar) (/!! vector-element-pvar scalar-pvar))
	  vector-pvar))

(defun vector-normal!! (vector-pvar1 vector-pvar2)
  (vscale-to-unit-vector!!
    (cross-product!! vector-pvar1 vector-pvar2)))
