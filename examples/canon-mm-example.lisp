;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *LISP; -*-

(in-package '*lisp)


;;; We assume a two-dimensional square vp set.
;;; The matrices to be multiplied are stored
;;; one element per virtual processor.  They
;;; must be single float numbers.


;;; This tells the *Lisp compiler that the function, when
;;; called, will return a single float pvar.


(*proclaim '(ftype (function (single-float-pvar single-float-pvar) single-float-pvar)
		   sf-canon-matrix-multiply
		   ))


;;; Assume the matrices look like

;;; a b c
;;; d e f
;;; g h i

;;; A B C
;;; D E F
;;; G H I

;;; as an example.


(defun sf-canon-matrix-multiply (m1 m2)

  ;; Tell the *Lisp compiler that m1 and m2 are
  ;; pvars of type single-float.

  (declare (type single-float-pvar m1 m2))

  (unless
    (and (eql 2 (vp-set-rank *current-vp-set*))
	 (eql (elt (vp-set-dimensions *current-vp-set*) 0)
	      (elt (vp-set-dimensions *current-vp-set*) 1)
	      ))
    (error "This only works in a 2-dimensional square Vp Set")
    )

  ;; Allocate some temporaries for our matrices.
  ;; Allocate temporaries to hold each processor's
  ;; X and Y addresses, since we will make use
  ;; of those addresses more than once.

  (*let (temp1 temp2 temp3 dest selfx selfy)
    (declare (type single-float-pvar temp1 temp2 temp3 dest))
    (declare (type (field-pvar 32) selfx selfy))

    ;;    (unless (= shit 1)
    (*set selfx (self-address-grid!! 0))
    (*set selfy (self-address-grid!! 1))
      ;;      )

    ;; What the *pset's do is make m1 look like
    ;;
    ;; a b c
    ;; e f d
    ;; i g h
    ;;
    ;; and m2 look like
    ;;
    ;; A E I
    ;; D H C
    ;; G B F
    ;;
    ;; that is, each row of m1 get's rotated in X by
    ;; an amount, and each column of m2 get's rotated
    ;; in Y by some amount.

    ;; The rotated m1 gets stored in temp1, and the
    ;; rotated m2 gets stored in temp2.

    (*pset :no-collisions m1 temp1
	   (grid!!
	     (mod!! (-!! selfx selfy) (the fixnum (dimension-size 0)))
	     selfy
	     )
	   :vp-set *current-vp-set*
	   )

    (*pset :no-collisions m2 temp2
	   (grid!!
	     (mod!! (-!! selfy selfx) (the fixnum (dimension-size 1)))
	     selfx
	     )
	   :vp-set *current-vp-set*
	   )

    (*set dest 0)

    ;; Now we multiply temp1 by temp2 and accumulate
    ;; the result in dest.  Then we rotate temp1 in X
    ;; by one and rotate temp2 in Y by 1.

    ;; We continue this process as many times as there
    ;; are elements in a single row/column.

    ;; After the first step dest looks like
    ;;
    ;; Aa Eb Ic
    ;; De Hf Cd
    ;; Gi Bg Fh
    ;;
    ;; and temp1 and temp2 look like
    ;;
    ;; b c a
    ;; f d e
    ;; g h i
    ;;
    ;; D H C
    ;; G B F
    ;; A E I
    ;;

    (dotimes (j (dimension-size 0))
      ;;(incf count)
      (*set temp3 (*!! temp1 temp2))
      (*set dest (+!! dest temp3))
      (*set temp1 (news!! temp1 1 0))
      (*set temp2 (news!! temp2 0 1))
      )

    ;; After the second step we get
    ;;
    ;; Aa + Db  Eb + Hc  Ic + Ca
    ;; De + Gf  Hf + Bd  Cd + Fe
    ;; Gi + Ag  Bg + Eh  Fh + Ii
    ;;
    ;; c a b
    ;; d e f
    ;; h e g
    ;;
    ;; G B F
    ;; A E I
    ;; D H C
    ;;
    ;; After the third step dest looks like
    ;;
    ;; Aa + Db + Gc  Eb + Hc + Ba  Ic + Ca + Fb
    ;; De + Gf + Ad  Hf + Bd + Ee  Cd + Fe + If
    ;; Gi + Ag + Dh  Bg + Eh + Hi  Fh + Ii + Cg
    ;;
    ;; and we are done.

    dest

    ))


(defun example-canon ()
  ;; Should print out a 5x5 grid of 128.0's
  (let-vp-set (mm-vp-set (create-vp-set #+*LISP-HARDWARE '(128 128) #+*LISP-SIMULATOR '(8 8)))
    (*with-vp-set mm-vp-set
      (*let ((m1 1.0) (m2 1.0))
	(declare (type single-float-pvar m1 m2))
	(ppp (sf-canon-matrix-multiply m1 m2) :mode :grid :end '(5 5))
	))))
