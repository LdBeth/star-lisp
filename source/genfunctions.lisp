;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Muser: yes -*-

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


;;; This file contains macros that expand into function definitions.  These
;;; functions are simple, parallel extensions of Common Lisp functions.


(defun set-vector-conditionally (vector-to-set value)
  (let ((any-set nil))
    (do-for-selected-processors-internal (j) (setq any-set t) (setf (aref vector-to-set j) value))
    any-set
    ))


(defun trivial-name (function-name)
  (intern
    (concatenate 'string "TRIVIAL-" (symbol-name function-name))
    (find-package '*sim-i)
   ))


(defmacro note-trivial-macro (&rest symbols)
  `(eval-when (:compile-toplevel :load-toplevel :execute) (mapcar 'note-trivial '(,@symbols)))
  )
(defun note-trivial (symbol) (setf (get symbol 'trivialp) t))
(defun symbol-trivial-p (symbol) (get symbol 'trivialp))

(defmacro def-trivial-one-arg-*lisp-functions (names-and-lisp-functions)
  `(progn
     (defun execute-trivial-one-arg-*lisp-function (name internal-name arg)
       (safety-check (new-pvar-check arg name))
       (let* ((return-pvar (allocate-temp-general-pvar))
              (return-array (pvar-array return-pvar))
              (arg-array (pvar-array arg))
              )
         (when (funcall internal-name arg-array return-array)
           (make-non-void return-pvar)
           )
         return-pvar
         ))
     ,@(mapcar
         #'(lambda (name-and-lisp-function)
             (let* ((name (car name-and-lisp-function))
                    (lisp-function (cadr name-and-lisp-function))
                    (internal-name (trivial-name name))
                    )
               `(defun ,internal-name (pvar-array return-array)
                  (let ((pvar-array pvar-array) (return-array return-array))
                    (with-simple-vectors (pvar-array return-array)
                      (let ((any-set nil))
                        (do-for-selected-processors-internal (j)
                          (setq any-set t)
                          (setf (aref return-array j) (,lisp-function (aref pvar-array j)))
                          )
                        any-set
                        ))))))
         names-and-lisp-functions
         )
     ,@(mapcar
         #'(lambda (name-and-lisp-function)
             (let* ((name (car name-and-lisp-function))
                    ;;(lisp-function (cadr name-and-lisp-function))
                    (internal-name (trivial-name name))
                    )
               `(defun ,name (pvar)
                  ;;(declare-pvar-arglist ,lisp-function)
                  ,(if (or (eql name 'byte-position!!)
                           (eql name 'byte-size!!))
                       '(pvar-argument!! pvar *bytespec)
                     '(simple-pvar-argument!! pvar))
                  (execute-trivial-one-arg-*lisp-function ',name ',internal-name pvar))
               ))
         names-and-lisp-functions
         )
     (note-trivial-macro ,@(mapcar #'car names-and-lisp-functions))
     ))

(defmacro def-trivial-two-arg-*lisp-functions (names-and-lisp-functions)
  `(progn
     (defun execute-trivial-two-arg-*lisp-function (name internal-name arg1 arg2)
       (safety-check (new-two-pvar-check arg1 arg2 name))
       (let* ((return-pvar (allocate-temp-general-pvar))
              (return-array (pvar-array return-pvar))
              (arg1-array (pvar-array arg1))
              (arg2-array (pvar-array arg2))
              )
         (when (funcall internal-name arg1-array arg2-array return-array)
           (make-non-void return-pvar)
           )
         return-pvar
         ))
     ,@(mapcar
         #'(lambda (name-and-lisp-function)
             (let* ((name (car name-and-lisp-function))
                    (lisp-function (cadr name-and-lisp-function))
                    (internal-name (trivial-name name))
                    )
               `(defun ,internal-name (pvar1-array pvar2-array return-array)
                  (let ((pvar1-array pvar1-array)
                        (pvar2-array pvar2-array)
                        (return-array return-array)
                        )
                    (with-simple-vectors (pvar1-array pvar2-array return-array)
                      (let ((any-set nil))
                        (do-for-selected-processors-internal (j)
                          (setq any-set t)
                          (setf (aref return-array j)
                            (,lisp-function (aref pvar1-array j) (aref pvar2-array j)))
                          )
                        any-set
                        ))))))
         names-and-lisp-functions
         )
     ,@(mapcar
         #'(lambda (name-and-lisp-function)
             (let* ((name (car name-and-lisp-function))
                    ;;(lisp-function (cadr name-and-lisp-function))
                    (internal-name (trivial-name name))
                    )
               `(defun ,name (pvar1 pvar2)
                  ;; (declare-pvar-arglist ,lisp-function)
                  ,(case name
                     ((ldb!! ldb-test!! mask-field!!)
                      '(pvar-argument!! pvar1 *bytespec pvar2 legal-pvar-value))
                     (otherwise '(simple-pvar-argument!! (pvar1 pvar2))))
                  (execute-trivial-two-arg-*lisp-function ',name ',internal-name pvar1 pvar2))
               ))
         names-and-lisp-functions
         )
     (note-trivial-macro ,@(mapcar #'car names-and-lisp-functions))
     ))


;; Functions like truncate which take one argument and optionally
;; a second.


(defmacro def-trivial-optional-two-arg-*lisp-functions (names-and-lisp-functions)
  `(progn
     (defun execute-trivial-optional-two-arg-*lisp-function
         (name internal-name arg1 &optional arg2)
       (safety-check (new-pvar-check arg1 name))
       (safety-check (and arg2 (new-pvar-check arg2 name)))
       (let* ((return-pvar (allocate-temp-general-pvar))
              (return-array (pvar-array return-pvar))
              (arg1-array (pvar-array arg1))
              (arg2-array (and arg2 (pvar-array arg2)))
              )
         (when (funcall internal-name arg1-array arg2-array return-array)
           (make-non-void return-pvar)
           )
         return-pvar
         ))
     ,@(mapcar
         #'(lambda (name-and-lisp-function)
             (let* ((name (car name-and-lisp-function))
                    (lisp-function (cadr name-and-lisp-function))
                    (internal-name (trivial-name name))
                    )
               `(defun ,internal-name (pvar1-array pvar2-array return-array)
                  (let ((pvar1-array pvar1-array)
                        (pvar2-array pvar2-array)
                        (return-array return-array)
                        )
                    (with-simple-vectors (pvar1-array return-array)
                      (let ((any-set nil))
                        (do-for-selected-processors-internal (j)
                          (setq any-set t)
                          (setf (aref return-array j)
                            (if pvar2-array
                                (,lisp-function (aref pvar1-array j) (aref pvar2-array j))
                              (,lisp-function (aref pvar1-array j))
                              )))
                        any-set
                        ))))))
         names-and-lisp-functions
         )
     ,@(mapcar
         #'(lambda (name-and-lisp-function)
             (let* ((name (car name-and-lisp-function))
                    ;;(lisp-function (cadr name-and-lisp-function))
                    (internal-name (trivial-name name))
                    )
               `(defun ,name (pvar1 &optional pvar2)
                  ;;(declare-pvar-arglist ,lisp-function)
                  (simple-pvar-argument!! pvar1 &opt pvar2)
                  (execute-trivial-optional-two-arg-*lisp-function
                   ',name ',internal-name pvar1 pvar2))
               ))
         names-and-lisp-functions
         )
     (note-trivial-macro ,@(mapcar #'car names-and-lisp-functions))
     ))


(defmacro def-trivial-three-arg-*lisp-functions (names-and-lisp-functions)
  `(progn
     (defun execute-trivial-three-arg-*lisp-function (name internal-name arg1 arg2 arg3)
       (safety-check (new-two-pvar-check arg1 arg2 name))
       (let* ((return-pvar (allocate-temp-general-pvar))
              (return-array (pvar-array return-pvar))
              (arg1-array (pvar-array arg1))
              (arg2-array (pvar-array arg2))
              (arg3-array (pvar-array arg3))
              )
         (when (funcall internal-name arg1-array arg2-array arg3-array return-array)
           (make-non-void return-pvar)
           )
         return-pvar
         ))
     ,@(mapcar
         #'(lambda (name-and-lisp-function)
             (let* ((name (car name-and-lisp-function))
                    (lisp-function (cadr name-and-lisp-function))
                    (internal-name (trivial-name name))
                    )
               `(defun ,internal-name (pvar1-array pvar2-array pvar3-array return-array)
                  (let ((pvar1-array pvar1-array)
                        (pvar2-array pvar2-array)
                        (pvar3-array pvar3-array)
                        (return-array return-array)
                        )
                    (with-simple-vectors (pvar1-array pvar2-array pvar3-array return-array)
                      (let ((any-set nil))
                        (do-for-selected-processors-internal (j)
                          (setq any-set t)
                          (setf (aref return-array j)
                            (,lisp-function (aref pvar1-array j) (aref pvar2-array j) (aref pvar3-array j))
                            ))
                        any-set
                        ))))))
         names-and-lisp-functions
         )
     ,@(mapcar
         #'(lambda (name-and-lisp-function)
             (let* ((name (car name-and-lisp-function))
                    ;;(lisp-function (cadr name-and-lisp-function))
                    (internal-name (trivial-name name))
                    )
               `(defun ,name (pvar1 pvar2 pvar3)
                  ;;(declare-pvar-arglist ,lisp-function)
                  ,(case name
                     ((deposit-field!! dpb!!)
                      '(pvar-argument!! pvar2 *bytespec (pvar1 pvar3) integer))
                     (otherwise '(simple-pvar-argument!! (pvar1 pvar2 pvar3))))
                  (execute-trivial-three-arg-*lisp-function ',name ',internal-name pvar1 pvar2 pvar3))
               ))
         names-and-lisp-functions
         )
     (note-trivial-macro ,@(mapcar #'car names-and-lisp-functions))
     ))


;; functions which take an arbitrary number of arguments.
;; Whether they allow none is a flag to the macro.
;; If they allow none, what the function returns given that
;; no arguments are provided is a parameter of the macro.


(defmacro def-trivial-nary-*lisp-functions
    
    (name-lispname-zeroargsp-zeroargsdefault-list)
  
  `(progn
     
     (defun execute-trivial-nary-*lisp-function
         
         (name internal-name lisp-function allow-zero-args zero-args-default &rest args)
       
       (safety-check (new-multiple-pvar-check args name))
       (let* ((nargs (length args))
              (return-pvar (allocate-temp-general-pvar))
              (return-array (pvar-array return-pvar))
              (arg-arrays (mapcar #'pvar-array args))
              (any-set nil)
              )
         
         (cond
          
          ((eql 0 nargs) 
           (if allow-zero-args
               (setq any-set (set-vector-conditionally return-array zero-args-default))
             (error "Zero arguments not allowed to ~S~%" name)
             ))
          
          ;; this is inefficient since it uses funcall, but
          ;; presumably calling an n-ary function with one argument
          ;; is a rare event
          
          ((eql 1 nargs)
           (let ((x-array (car arg-arrays)))
             (with-simple-vectors (x-array)
               (do-for-selected-processors-internal (j)
                 (setq any-set t)
                 (let ((element (aref x-array j)))
                   (setf (aref return-array j) (funcall lisp-function element))
                   )))))
          
          ;; for multiple arguments set up the return array with
          ;; values from the first argument, then call the internal
          ;; function to combine all the rest of the arguments with
          ;; the return array values.
          
          (t
           (let* ((first-array (car arg-arrays)))
             (with-simple-vectors (first-array)
               (do-for-selected-processors-internal (j)
                 (setq any-set t)
                 (setf (aref return-array j) (aref first-array j))
                 )
               (apply internal-name return-array (cdr arg-arrays))
               ))))
         
         (when any-set (make-non-void return-pvar))
         return-pvar
         
         ))
     
     ,@(mapcar
         #'(lambda (name lisp-function)
             (let ((internal-name (trivial-name name)))
               `(defun ,internal-name (return-array &rest pvar-arrays)
                  (let ((return-array return-array))
                    (when pvar-arrays
                      (let ((next-array (car pvar-arrays)))
                        (with-simple-vectors (return-array next-array)
                          (do-for-selected-processors (j)
                            (setf (aref return-array j)
                              (,lisp-function (aref return-array j) (aref next-array j)))
                            )))
                      (apply ',internal-name return-array (cdr pvar-arrays))
                      )))))
         (mapcar #'first name-lispname-zeroargsp-zeroargsdefault-list)
         (mapcar #'second name-lispname-zeroargsp-zeroargsdefault-list)
         )
     
     ,@(mapcar
         #'(lambda (name lisp-function allow-zero-args zero-args-default)
             (let ((internal-name (trivial-name name)))
               `(defun ,name (&rest pvars)
                  ;;(declare-pvar-arglist ,lisp-function)
                  (simple-pvar-argument!! &rest pvars)
                  (apply #'execute-trivial-nary-*lisp-function
                         ',name ',internal-name ',lisp-function
                         ,allow-zero-args ,zero-args-default pvars
                         ))))
         (mapcar #'first name-lispname-zeroargsp-zeroargsdefault-list)
         (mapcar #'second name-lispname-zeroargsp-zeroargsdefault-list)
         (mapcar #'third name-lispname-zeroargsp-zeroargsdefault-list)
         (mapcar #'fourth name-lispname-zeroargsp-zeroargsdefault-list)
         )
     
     (note-trivial-macro ,@(mapcar #'car name-lispname-zeroargsp-zeroargsdefault-list))
     
     ))



;; Functions that take pvars and combine all the active elements of the
;; pvar in some way to produce a single result.  The initial-value is
;; usually the identity element for the operation.  rval-if-none-active
;; is what is returned if no processors are active.

(defmacro def-trivial-*lisp-reduction-function-using-initial-value
    (name lisp-function initial-value rval-if-none-active)
  `(*defun 
    ,name (pvar)
    ;;(declare-pvar-arglist ,lisp-function)	   
    (simple-pvar-argument!! pvar)
    (safety-check (new-pvar-check pvar ',name))
    (let ((any-active nil)
          (return-value ,initial-value)
          (x-array (pvar-array pvar))
          )
      (with-simple-vectors (x-array)
        (do-for-selected-processors-internal (j)
          (setq any-active t)
          (setq return-value (,lisp-function return-value (aref x-array j)))
          )
        (if any-active return-value ,rval-if-none-active)
        ))))


;; Similar to def-trivial-*lisp-reduction-function-using-initial-value
;; except that no initial-value is provided.  Instead, the first
;; active element is used.

(defmacro def-trivial-*lisp-reduction-function-using-first-element
    (name lisp-function rval-if-none-active)
  `(*defun 
    ,name (pvar)
    ;;(declare-pvar-arglist ,lisp-function)
    (simple-pvar-argument!! pvar)
    (safety-check (new-pvar-check pvar ',name))
    (let ((first t)
          (return-value nil)
          (x-array (pvar-array pvar))
          )
      (with-simple-vectors (x-array)
        (do-for-selected-processors (j)
          (if first
              (progn (setq first nil) (setq return-value (aref x-array j)))
            (setq return-value (,lisp-function return-value (aref x-array j)))
            ))
        (if first ,rval-if-none-active return-value)
        ))))



;; Comparision functions like < and >.
;; Zero arguments are illegal, one argument always produces t!!.


(defmacro def-trivial-*lisp-comparision-functions (name-lisp-function-list)
  
  `(progn
     
     (defun execute-trivial-*lisp-comparision-function
         (name internal-name arglist)
       (safety-check (new-multiple-pvar-check arglist name))
       (let* ((nargs (length arglist))
              (return-pvar (allocate-temp-general-pvar))
              (return-array (pvar-array return-pvar))
              (argument-arrays (mapcar #'pvar-array arglist))
              (any-set nil)
              )
         (cond
          ((eql 0 nargs)
           (error "~S:  This function does not allow 0 arguments." name))
          ((eql 1 nargs)
           (setq any-set (set-vector-conditionally return-array t)))
          (t
           (setq any-set (set-vector-conditionally return-array t))
           (funcall internal-name return-array argument-arrays nargs))
          )
         (when any-set (make-non-void return-pvar))
         return-pvar
         ))
     
     ,@(mapcar
         #'(lambda (name lisp-function)
             (let ((internal-name (trivial-name name)))
               `(defun ,internal-name (return-array argument-arrays nargs)
                  (if (< nargs 2)
                      return-array
                    (let ((return-array return-array)
                          (arg1-array (car argument-arrays))
                          (arg2-array (cadr argument-arrays))
                          )
                      (with-simple-vectors (return-array arg1-array arg2-array)
                        (do-for-selected-processors (j)
                          (and (aref return-array j)
                               (setf (aref return-array j)
                                 (,lisp-function (aref arg1-array j) (aref arg2-array j))
                                 )))
                        (funcall ',internal-name return-array (cdr argument-arrays) (1- nargs))
                        ))))))
         (mapcar #'car name-lisp-function-list)
         (mapcar #'cadr name-lisp-function-list)
         )
     
     ,@(mapcar
         #'(lambda (name lisp-function)
             (declare (ignore lisp-function))
             (let ((internal-name (trivial-name name)))
               `(defun ,name (&rest pvars)
                  ;;(declare-pvar-arglist ,lisp-function)
                  (simple-pvar-argument!! &rest pvars)
                  (execute-trivial-*lisp-comparision-function
                   ',name ',internal-name pvars
                   ))))
         (mapcar #'car name-lisp-function-list)
         (mapcar #'cadr name-lisp-function-list)
         )
     
     (note-trivial-macro ,@(mapcar #'car name-lisp-function-list))
     
     ))



;;; Given a list of pvar function specifications (see trivfunctions.lisp)
;;; This macro macroexpands into a list of 'def-trivial...' macro
;;; definitions.  These definitions then macroexpand into the proper
;;; defun or *defun for each function.  Ah, the wonders of Lisp!

(defmacro define-trivial-functions (def-trivial-macro-name flist)
  (let ((macrolist nil))
    (dolist (j (eval flist))
      (push (cons def-trivial-macro-name j) macrolist)
     )
    `(progn ,@macrolist)
   ))


(defun arglist-symbol-to-pvar-symbol (symbol)
  (cond
    ((member symbol lambda-list-keywords :test #'eq) symbol)
    ((listp symbol) (arglist-symbol-to-pvar-symbol (car symbol)))
    (t
     (let ((string (symbol-name symbol)))
       (let ((length (length string)))
	 (if (char-equal (char string (1- length)) #\S)
	     (intern (concatenate 'string (subseq string 0 (1- length)) "-PVARS") '*sim)
	     (intern (concatenate 'string string "-PVAR") '*sim)
	     ))))))

;;; (defun common-lisp-arglist-to-starlisp-arglist (common-lisp-function)
;;;   #-(OR SYMBOLICS LCL3.0)
;;;   (declare (ignore common-lisp-function))
;;;   (mapcar
;;;     'arglist-symbol-to-pvar-symbol
;;;     #+SYMBOLICS
;;;     (scl:arglist common-lisp-function)
;;;     #+LCL3.0
;;;     (sys::arglist common-lisp-function)
;;;     #-(OR SYMBOLICS LCL3.0)
;;;     nil
;;;     ))
;;; 
;;; (defmacro declare-pvar-arglist (common-lisp-function)
;;;   (let ((arglist (common-lisp-arglist-to-starlisp-arglist common-lisp-function)))
;;;     #+SYMBOLICS
;;;     `(declare (scl:arglist ,@arglist))
;;;     #+LCL3.0
;;;     `(declare (sys::arglist ,arglist))
;;;     #-(OR SYMBOLICS LCL3.0)
;;;     (declare (ignore arglist))
;;;     #-(OR SYMBOLICS LCL3.0)
;;;     nil
;;;     ))
;;; 