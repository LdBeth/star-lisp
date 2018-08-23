;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Muser: Yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(progn


(def-trivial-one-arg-*lisp-functions (
   							       
   (1+!! 1+)
   (1-!! 1-)
   (booleanp!! booleanp)
   (floatp!! floatp)
   (integerp!! integerp)
   (complexp!! complexp)
   (numberp!! numberp)
   (characterp!! characterp)
   (signum!! signum)
   (zerop!! zerop)
   (oddp!! oddp)
   (evenp!! evenp)
   (plusp!! plusp)
   (minusp!! minusp)
   (realpart!! realpart)
   (imagpart!! imagpart)
   (phase!! phase)
   (conjugate!! conjugate)
   (cis!! cis)
   (not!! not)
   (null!! null)
   (isqrt!! isqrt)
   (lognot!! lognot)
   (random!! random)
   (abs!! abs)
   (sin!! sin)
   (cos!! cos)
   (tan!! tan)
   (exp!! exp)
   (logcount!! logcount)
   (integer-length!! integer-length)
   (standard-char-p!! standard-char-p)
   (graphic-char-p!! graphic-char-p)
   (string-char-p!! string-char-p)
   (alpha-char-p!! alpha-char-p)
   (upper-case-p!! upper-case-p)
   (lower-case-p!! lower-case-p)
   (both-case-p!! both-case-p)
   (alphanumericp!! alphanumericp)
   (char-code!! char-code)
   (char-upcase!! char-upcase)
   (char-downcase!! char-downcase)
   (char-flipcase!! char-flipcase)
   (character!! character)
   (char-int!! char-int)

   (sqrt!! starlisp-sqrt)
   (asin!! starlisp-asin)
   (acos!! starlisp-acos)
   (acosh!! starlisp-acosh)
   (atanh!! starlisp-atanh)

   (sinh!! sinh)
   (cosh!! cosh)
   (tanh!! tanh)
   (asinh!! asinh)
   
   (byte-size!! byte-size)
   (byte-position!! byte-position)

   (gray-code-from-integer!! front-end-gray-code-from-integer)
   (integer-from-gray-code!! front-end-integer-from-gray-code)

  ))


(def-trivial-two-arg-*lisp-functions (

  (eq!! eq)
  (eql!! eql)
  (mod!! mod)
  (rem!! rem)
  (ash!! ash)
  (expt!! starlisp-expt)
  (lognand!! lognand)
  (lognor!! lognor)
  (logandc1!! logandc1)
  (logandc2!! logandc2)
  (logorc1!! logorc1)
  (logorc2!! logorc2)
  (logtest!! logtest)
  (logbitp!! logbitp)
  (mask-field!! mask-field)
  (byte!! byte)
  (ldb!! ldb)
  (ldb-test!! ldb-test)
  (scale-float!! scale-float)
  (compare!! compare)

 ))


(def-trivial-optional-two-arg-*lisp-functions (

  (float!! float)
  (truncate!! truncate)
  (ceiling!! ceiling)
  (floor!! floor)
  (round!! round)
  (complex!! complex)
  (atan!! atan)
  (ffloor!! ffloor)
  (fceiling!! fceiling)
  (ftruncate!! ftruncate)
  (fround!! fround)
  (float-sign!! float-sign)
  (digit-char-p!! digit-char-p)
  (log!! starlisp-log)

 ))


(def-trivial-three-arg-*lisp-functions (
					
  (load-byte!! load-byte)
  (dpb!! dpb)
  (deposit-field!! deposit-field)
  (boole!! boole)

  ))

(def-trivial-nary-*lisp-functions (

   (+!! + t 0)
   (-!! - nil 0)
   (*!! * t 1)
   (internal-/!! / nil 0)
   (logand!! logand t -1)
   (logior!! logior t 0)
   (logxor!! logxor t 0)
   (logeqv!! logeqv t -1)
   (min!! min nil nil)
   (max!! max nil nil)
   (gcd!! gcd t 0)
   (lcm!! lcm nil 0)

  ))


(define-trivial-functions
  def-trivial-*lisp-reduction-function-using-initial-value

  '((*sum + 0 0)
    (*logand logand -1 -1)
    (*logior logior 0 0)
    (*logxor logxor 0 0)
   ))


(define-trivial-functions
  def-trivial-*lisp-reduction-function-using-first-element

  '((*min min nil)
    (*max max nil)
   ))



(def-trivial-*lisp-comparision-functions (
					  
  (=!! =)
  (<!! <)
  (>!! >)
  (<=!! <=)
  (>=!! >=)
  (char=!! char=)
  (char<!! char<)
  (char>!! char>)
  (char<=!! char<=)
  (char>=!! char>=)
  (char-equal!! char-equal)
  (char-lessp!! char-lessp)
  (char-greaterp!! char-greaterp)
  (char-not-lessp!! char-not-lessp)
  (char-not-greaterp!! char-not-greaterp)

 ))

)
