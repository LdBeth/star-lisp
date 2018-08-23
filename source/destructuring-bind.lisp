;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *sim-i; MUSER: YES; -*-

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


(proclaim '(special destructuring-bind-error-tag))

(defun parse-body (body &optional environment documentationp)
  (do ((body body (cdr body))
       (declares ())
       (documentation nil)
       (form nil))
      ((null body)
       (values () (nreverse declares) documentation))
    (setq form (car body))
    (when (not (and (consp form) (eq (car form) 'declare)))
      (setq form (parse-body-macroexpand-1 form environment)))
    (if (and (consp form) (eq (car form) 'declare))
	(push form declares)
	(if (and (stringp form) documentationp (not documentation) (cdr body))
	    (setq documentation form)
	    (return (values body (nreverse declares) documentation))))))


;;; Kyoto Common Lisp insists that the &environment thingee
;;; be the first argument of a lambda list.   This is used
;;; to port this code for the Starlisp Simulator.  JP.


#-(or SYMBOLICS lucid)
(defmacro macro-destructuring-bind
	  #-KCL
	  (lambda-list form &body body &environment environment)
	  #+KCL
	  (&environment environment lambda-list form &body body)
  (multiple-value-bind (body declares)
      (parse-body body environment t)
    (multiple-value-bind (bindings extra)
	(decompose lambda-list '%form% nil 'nil 'macro-destructuring-bind t)
      `(let* ((%form% ,form)
	      ,@bindings)
	 ,@declares
	 ,@extra
	 ,@body))))

#-(or SYMBOLICS lucid)
(defmacro destructuring-bind
	  #-KCL
	  (lambda-list form &body body &environment environment)
	  #+KCL
	  (&environment environment lambda-list form &body body)
  (multiple-value-bind (body declares)
      (parse-body body environment t)
    (multiple-value-bind (bindings extra)
	(decompose lambda-list '%form% nil 'nil 'destructuring-bind nil :top-level-bindings t)
      `(let* ((%form% ,form)
	      ,@bindings)
	 ,@declares
	 ,@extra
	 ,@body))))

#-(or SYMBOLICS lucid)

(progn


(defmacro hacked-destructuring-bind
	  #-KCL
	  (lambda-list form &body body &environment environment)
	  #+KCL
	  (&environment environment lambda-list form &body body)
  (multiple-value-bind (body declares)
      (parse-body body environment t)
    (multiple-value-bind (bindings extra)
	(decompose lambda-list '%form% nil 'nil 'destructuring-bind nil :top-level-bindings t
		   :getf 'hacked-getf :keywordp 'keywordp-reference)
      `(let* ((%form% ,form)
	      ,@bindings)
	 ,@declares
	 ,@extra
	 ,@body))))

(defmacro deftype-destructuring-bind
	  #-KCL
	  (lambda-list form &body body &environment environment)
	  #+KCL
	  (&environment environment lambda-list form &body body)
  (multiple-value-bind (body declares)
      (parse-body body environment t)
    (multiple-value-bind (bindings extra)
	(decompose lambda-list '%form% nil 'nil 'deftype nil :optional-initial-value ''*)
      `(let* ((%form% ,form)
	      ,@bindings)
	 ,@declares
	 ,@extra
	 ,@body))))



(defvar *name* nil)
(defvar *type* nil)

(defun too-many-forms (form)
  (if destructuring-bind-error-tag
      (progn (compiler-warn nil "Too many arguments ~{~S~^, ~} to ~S."
			    (mapcar #'slc-node-reference-or-arg form)
			    *name*)
	     (throw destructuring-bind-error-tag nil))
      (error "Too many forms ~S to~@[ ~S~] ~S." form *type* *name*)))

(defun not-enough-forms ()
  (if destructuring-bind-error-tag
      (progn (compiler-warn nil "Not enough arguments were provided to ~S." *name*)
	     (throw destructuring-bind-error-tag nil))
      (error "Not enough forms were provided to~@[ ~S~] ~S." *type* *name*)))

(defun check-keys (form keys allow-other-keys &optional (keywordp nil) (key nil))
  (if (null keywordp) (setq keywordp 'keywordp))
  (if (null key)
      (setq key (if (eq keywordp 'keywordp-reference) (intern "NODE-REFERENCE" (find-package 'slc)) 'identity))
      )
  (do ((form form (cddr form))
       (invalid-keys ()))
      ((null form)
       (if (and invalid-keys (not allow-other-keys))
	   (if destructuring-bind-error-tag
	       (progn (compiler-warn (car invalid-keys) "Invalid keyword given to &key interface for function ~S." *name*)
		      (throw destructuring-bind-error-tag nil))
	       (error "Invalid keywords ~{~S~^, ~} were passed to~@[ ~S~] ~S." invalid-keys *type* *name*))))
    (unless (funcall keywordp (car form)) ; Add variable throw keyword-expression for transforms.
      (if destructuring-bind-error-tag
	  (progn (return nil)) ; ????
	  (progn (error "A non keyword ~S was given to~@[ ~S~] ~S." (car form) *type* *name*))))
    (unless (cdr form)
      (if destructuring-bind-error-tag
	  (progn (compiler-warn nil "Wrong number of &key arguments to ~S." *name*)
		 (throw destructuring-bind-error-tag nil))
	  (error "Odd number of arguments were provided to~@[ ~S~] ~S." *type* *name*)))
    (if (not allow-other-keys)
	(if (eq (funcall key (car form)) :allow-other-keys)
	    (setq allow-other-keys (funcall key (cadr form)))
	    (unless (member (funcall key (car form)) keys)
	      (push (funcall key (car form)) invalid-keys))))))

(defun lambda-keyword-p (object)
  #+lucid (declare (special lambda-list-keywords))
  (member object lambda-list-keywords))

(defvar *getf-function* 'getf)
(defvar *keywordp-function* nil)
(defvar *optional-initial-value* nil)

(defun decompose (lambda-list form environment &optional name (type 'macro) (top-level t)
			      &key ((:getf *getf-function*) 'getf)
			           ((:keywordp *keywordp-function*) nil)
				   top-level-bindings
				   ((:optional-initial-value *optional-initial-value*)))
  (declare (ignore type name))
  (cond ((null lambda-list)
	 (values (if (or top-level top-level-bindings)
		     `(#| (*name* ',name) (*type* ',type) |#)
		     ())
		 `((if ,(if top-level `(cdr ,form) form)
		       (too-many-forms ,form)))))
	((atom lambda-list)
	 (values (if top-level
		     `(#|(*name* ',name) (*type* ',type)|# (,lambda-list (cdr ,form)))
		     (if top-level-bindings
			 `(#|(*name* ',name) (*type* ',type)|# (,lambda-list ,form))
			 `((,lambda-list ,form))))
		 ()))
	((lambda-keyword-p (car lambda-list))
	 (multiple-value-bind (bindings extra)
	     (decompose-&whole lambda-list form environment top-level)
	   (values (if (or top-level top-level-bindings)
		       `(#|(*name* ',name) (*type* ',type)|# ,@bindings)
		       bindings)
		   extra)))
	(t (multiple-value-bind (bindings extra)
	       (decompose-required lambda-list form environment t)
	     (values (if top-level
			 `(#|(*name* ',name) (*type* ',type)|# (,form (cdr ,form)) ,@bindings)
			 (if top-level-bindings
			     `(#|(*name* ',name) (*type* ',type)|# ,@bindings)
			     bindings))
		     extra)))))

(defun decompose-&whole (lambda-list form environment top-level)
  (if (eq (car lambda-list) '&whole)
      (if (cdr lambda-list)
	  (multiple-value-bind (bindings extra)
	      (cond ((null (cddr lambda-list))
		     (values () ()))
		    ((atom (cddr lambda-list))
		     (values `((,(cddr lambda-list) ,form))
			     ()))
		    ((lambda-keyword-p (caddr lambda-list))
		     (decompose-&environment (cddr lambda-list) form environment nil))
		    (t (decompose-required (cddr lambda-list) form environment nil)))
	    (values (if (and top-level bindings)
			`((,(cadr lambda-list) (prog1 ,form (setq ,form (cdr ,form))))
			  ,@bindings)
			`((,(cadr lambda-list) ,form)
			  ,@bindings))
		    extra))
	  (error "&Whole must be followed by a symbol in lambda-list."))
      (multiple-value-bind (bindings extra)
	  (decompose-&environment lambda-list form environment t)
	(values (if top-level
		    `((,form (cdr ,form))
		      ,@bindings)
		    bindings)
		extra))))

(defun decompose-&environment (lambda-list form environment checkp)
  (if (eq (car lambda-list) '&environment)
      (if (cdr lambda-list)
	  (multiple-value-bind (bindings extra)
	      (cond ((null (cddr lambda-list))
		     (values () ()))
		    ((atom (cddr lambda-list))
		     (values `((,(cddr lambda-list) ,form))
			     ()))
		    ((lambda-keyword-p (caddr lambda-list))
		     (decompose-&optional (cddr lambda-list) form environment checkp))
		    (t (decompose-required (cddr lambda-list) form environment checkp)))
	    (values `((,(cadr lambda-list) ,environment)
		      ,@bindings)
		    extra))
	  (error "&Environment must be followed by a symbol in lambda-list."))
      (decompose-&optional lambda-list form environment checkp)))

(defun decompose-required (lambda-list form environment checkp &aux first)
  (cond ((null lambda-list)
	 (values ()
		 (if checkp
		     `((if ,form (too-many-forms ,form)))
		     ())))
	((atom lambda-list)
	 (values `((,lambda-list ,form))
		 ()))
	((lambda-keyword-p (setq first (car lambda-list)))
	 (decompose-&optional lambda-list form environment checkp))
	((consp first)
	 (let ((gensym (gensym "DESTRUCTURE-")))
	   (multiple-value-bind (bindings extra)
	       (decompose first gensym environment nil nil nil)
	     (multiple-value-bind (more-bindings more-extra)
		 (decompose-required (cdr lambda-list) form environment t)
	       (values `((,gensym (if ,form (pop ,form) (not-enough-forms)))
			 ,@bindings
			 ,@more-bindings)
		       `(,@extra
			 ,@more-extra))))))
	(t (multiple-value-bind (bindings more)
	       (decompose-required (cdr lambda-list) form environment t)
	     (values `((,(car lambda-list) (if ,form (pop ,form) (not-enough-forms)))
		       ,@bindings)
		     more)))))

(defun decompose-&optional (lambda-list form environment checkp)
  (if (eq (car lambda-list) '&optional)
      (decompose-optional (cdr lambda-list) form environment checkp)
      (decompose-&rest lambda-list form environment checkp)))

(defun decompose-optional (lambda-list form environment checkp &aux first)
  (cond ((null lambda-list)
	 (values ()
		 (if checkp
		     `((if ,form (too-many-forms ,form)))
		     ())))
	((atom lambda-list)
	 (values `((,lambda-list ,form))
		 ()))
	((lambda-keyword-p (setq first (car lambda-list)))
	 (decompose-&rest lambda-list form environment checkp))
	(t (let (var init svar)
	     (if (consp first)
		 (setq var (car first)
		       init (if (cdr first) (cadr first) *optional-initial-value*)
		       svar (caddr first))
		 (setq var first init *optional-initial-value* svar nil))
	     (if (consp var)
		 (let ((gensym (gensym "DESTRUCTURE-")))
		   (multiple-value-bind (bindings extra)
		       (decompose var gensym environment nil nil nil)
		     (multiple-value-bind (more-bindings more-extra)
			 (decompose-optional (cdr lambda-list) form environment t)
		       (values `(,@(if svar `((,svar (if ,form t))))
				 (,gensym (if ,form (pop ,form) ,init))
				 ,@bindings
				 ,@more-bindings)
			       `(,@extra
				 ,@more-extra)))))
		 (multiple-value-bind (bindings extra)
		     (decompose-optional (cdr lambda-list) form environment t)
		   (values `(,@(if svar `((,svar (if ,form t))))
			     (,var (if ,form (pop ,form) ,init))
			     ,@bindings)
			   extra)))))))

(defun decompose-&rest (lambda-list form environment checkp)
  (if (or (eq (car lambda-list) '&rest)
	  (eq (car lambda-list) '&body))
      (if (cdr lambda-list)
	  (multiple-value-bind (bindings extra)
	      (cond ((null (cddr lambda-list))
		     (values () ()))
		    ((atom (cddr lambda-list))
		     ;; (... &rest foo . bar) valid?
		     (values `((,(cddr lambda-list) ,form))
			     ()))
		    ((lambda-keyword-p (caddr lambda-list))
		     (decompose-&key (cddr lambda-list) form environment nil))
		    (t (error "Expected a lambda-keyword following &rest or &body in lambda-list but got ~S."
			      (caddr lambda-list))))
	    (values `((,(cadr lambda-list) ,form)
		      ,@bindings)
		    extra))
	  (error "&Rest or &body must be followed by a symbol in lambda-list."))
      (decompose-&key lambda-list form environment checkp)))

(defun decompose-&key (lambda-list form environment checkp)
  (if (eq (car lambda-list) '&key)
      (decompose-key (cdr lambda-list) form environment ())
      (decompose-&aux lambda-list form environment checkp)))

(defun decompose-key (lambda-list form environment keys-seen &aux first)
  (cond ((null lambda-list)
	 (values () `((check-keys ,form ',keys-seen nil
				  ,(if *keywordp-function* (list 'quote *keywordp-function*) ''keywordp)))))
	((atom lambda-list)
	 (error "Can not have dotted lambda-list after &key."))
	((lambda-keyword-p (setq first (car lambda-list)))
	 (decompose-&allow-other-keys lambda-list form environment keys-seen))
	(t (let (var keyword init svar)
	     (if (consp first)
		 (if (consp (car first))
		     (setq keyword (caar first) var (cadar first) init (cadr first) svar (caddr first))
		     (setq var (car first) keyword (intern (string var) 'keyword) init (cadr first) svar (caddr first)))
		 (setq var first keyword (intern (string var) 'keyword) init nil svar nil))
	     (if (consp var)
		 (let ((gensym (gensym "DESTRUCTURE-")))
		   (multiple-value-bind (bindings extra)
		       (decompose var gensym environment nil nil nil)
		     (multiple-value-bind (more-bindings more-extra)
			 (decompose-key (cdr lambda-list) form environment (cons keyword keys-seen))
		       (values `(,@(cond (svar
					  `((,svar nil)
					    (,gensym (if (eq (,*getf-function* ,form ,keyword '%default%) '%default%)
							 ,init
							 (progn (setq ,svar t) (,*getf-function* ,form ,keyword))))))
					 ((constantp init)
					  `((,gensym (,*getf-function* ,form ,keyword ,init))))
					 (t `((,gensym (if (eq (,*getf-function* ,form ,keyword '%default%) '%default%)
							   ,init
							   (,*getf-function* ,form ,keyword))))))
				 ,@bindings
				 ,@more-bindings)
			       `(,@extra
				 ,@more-extra)))))
		 (multiple-value-bind (bindings extra)
		     (decompose-key (cdr lambda-list) form environment (cons keyword keys-seen))
		   (values `(,@(cond (svar
				      `((,svar nil)
					(,var (if (eq (,*getf-function* ,form ,keyword '%default%) '%default%)
						  ,init
						  (progn (setq ,svar t) (,*getf-function* ,form ,keyword))))))
				     ((constantp init)
				      `((,var (,*getf-function* ,form ,keyword ,init))))
				     (t `((,var (if (eq (,*getf-function* ,form ,keyword '%default%) '%default%)
						    ,init
						    (,*getf-function* ,form ,keyword))))))
			     ,@bindings)
			   extra)))))))
				    
(defun decompose-&allow-other-keys (lambda-list form environment keys-seen)
  (if (eq (car lambda-list) '&allow-other-keys)
      (if (cdr lambda-list)
	  (if (atom (cdr lambda-list))
	      (error "Can not have dotted lambda-list following &allow-other-keys.")
	      (if (lambda-keyword-p (cadr lambda-list))
		  (multiple-value-bind (bindings extra)
		      (decompose-&aux (cdr lambda-list) form environment nil)
		    (values bindings
			    `((check-keys ,form ',keys-seen t
					  ,(if *keywordp-function* (list 'quote *keywordp-function*) ''keywordp))
			      ,@extra)))
		  (error "Expected a lambda keyword after &allow-other-keys, but got ~S." (cadr lambda-list))))
	  (values ()
		  `((check-keys ,form ',keys-seen t ,(if *keywordp-function* (list 'quote *keywordp-function*) ''keywordp)))))
      (multiple-value-bind (bindings extra)
	  (decompose-&aux lambda-list form environment nil)
	(values bindings
		`((check-keys ,form ',keys-seen nil ,(if *keywordp-function* (list 'quote *keywordp-function*) ''keywordp))
		  ,@extra)))))

(defun decompose-&aux (lambda-list form environment checkp)
  (if (eq (car lambda-list) '&aux)
      (multiple-value-bind (bindings extra)
	  (decompose-aux (cdr lambda-list) environment)
	(values bindings
		(if checkp
		    `((if ,form (too-many-forms ,form))
		      ,@extra)
		    extra)))
      (error "Invalid or misplaced lambda-keyword ~S in lambda-list." (car lambda-list))))

(defun decompose-aux (lambda-list environment &aux first)
  (cond ((null lambda-list)
	 (values () ()))
	((atom lambda-list)
	 (error "Can not have dotted lambda-list after &aux."))
	((lambda-keyword-p (setq first (car lambda-list)))
	 (error "Misplaced lambda-keyword ~S." first))
	(t (let (var init)
	     (if (consp first)
		 (setq var (car first) init (cadr first))
		 (setq var first init nil))
	     (if (consp var)
		 (let ((gensym (gensym "DESTRUCTURE-")))
		   (multiple-value-bind (bindings extra)
		       (decompose var gensym environment nil nil nil)
		     (multiple-value-bind (more-bindings more-extra)
			 (decompose-aux (cdr lambda-list) environment)
		       (values `((,gensym ,init)
				 ,@bindings
				 ,@more-bindings)
			       `(,@extra
				 ,@more-extra)))))
		 (multiple-value-bind (bindings extra)
		     (decompose-aux (cdr lambda-list) environment)
		   (values `((,var ,init)
			     ,@bindings)
			   extra)))))))



)
