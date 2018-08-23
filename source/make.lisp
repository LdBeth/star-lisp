;;; -*- Mode: LISP; Syntax: Common-lisp; Package: User; Base: 10 -*-


#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(user::ms user::msc user::mss user::mss-all user::make-system) 
	  (find-package :user)))|#
#+ccl
(defpackage :user
  (:export "MS" "MSC" "MSS" "MSS-ALL" "MAKE-SYSTEM"))

#+Allegro
(defpackage "CG-USER"
  (:import-from "USER" "MS" "MSC" "MSS" "MSS-ALL" "MAKE-SYSTEM"))

(in-package :user)

(defvar *all-possible-lisp-extensions* '(".cl" ".l" ".lsp" ".lisp" ""))

(defvar *binary-lisp-extension*
  #+(OR ALLEGRO :ALLEGRO CORMANLISP) ".fasl"
  #+CMU (concatenate 'string "." (c:backend-fasl-file-type c:*backend*))
  #+LISPWORKS ".fsl"
  #+:CLISP ".fas"
  #+SYMBOLICS ".bin" 
  #+KCL ".o"
  #+ccl (namestring ccl:*.fasl-pathname*)
  )

(defvar *defsystem-suffix* ".sys")
(defvar *alias-prefix-char-list* nil)
(defvar *alias-definition-list* nil)

;;; this is where the make-system code looks for system definition
;;; files.  Change this as appropriate to your system.

(defparameter *defsystem-search-path* '("/usr/local/etc/common-lisp"))

(defun errmsg (format-string &rest format-args)
  (terpri *error-output*)
  (apply 'format *error-output* format-string format-args)
  )

;;; set up mechanism so that 'logical names' (aliases) can be
;;; defined and undefined.  Aliases are recognized by beginning
;;; with a specific user-defined character.  This character is
;;; specified using define-alias-prefix-char.


;;; Actual aliases are defined using define-alias.
;;; Thus
;;; (define-alias-prefix-char #\&)
;;; (define-alias "HOST" "godot:")
;;;
;;; would cause the string "&HOST" to be interpreted as "godot:"
;;; in the context of the make-system file where appropriate.
;;;

(defun define-alias-prefix-char (char)
  (if (characterp char)
      (setq *alias-prefix-char-list*
	    (union (list char) *alias-prefix-char-list*))
      (error "DEFINE-ALIAS-PREFIX-CHAR:  Argument (~A) is not a character" char)
    ))

(defun undefine-alias-prefix-char (char)
  (setq *alias-prefix-char-list* (remove char *alias-prefix-char-list*))
 )


(defun undefine-alias (alias)
  (setq *alias-definition-list*
	(remove-if #'(lambda (assoc-pair) (equal (car assoc-pair) alias))
		   *alias-definition-list*
	 )))

(defun define-alias (alias real-string)
  (when (or (null (stringp alias)) (null (stringp real-string)))
    (error "DEFINE-ALIAS:  Arguments must be strings."))
  (undefine-alias alias)
  (push (list alias real-string) *alias-definition-list*)
  )

(defun convert-alias-to-real-string (alias)
  (let ((real-string
	  (cadr (assoc alias *alias-definition-list* :test #'string=))))
    (if (null real-string)
	(error "CONVERT-ALIAS-TO-REAL-STRING: Alias (~A) not defined." alias)
	real-string
     )))

(defun has-alias-prefix-char (string)
  (if (zerop (length string))
      nil
      (member (char string 0) *alias-prefix-char-list*)
   ))

(defun possibly-convert-string-using-alias-info (string)
  (if (has-alias-prefix-char string)
      (convert-alias-to-real-string (subseq string 1))
      string
   ))

(defun combine-string-list-to-form-full-file-path (string-list)
  (apply
   #'concatenate
   'string
   (mapcar
     #'(lambda (string)
         (if (null (stringp string))
             (error "All components must be strings")
           (possibly-convert-string-using-alias-info string)
           ))
     string-list
     )))

(defun source-code-file (base-file-name)

  ;; Given a file name, find a file with that name and some
  ;; lispy extension, as defined by *all-possible-lisp-extensions*

  (dolist (ext *all-possible-lisp-extensions*)
    (let ((full-name (concatenate 'string base-file-name ext)))
      (and (probe-file full-name) (return-from source-code-file full-name))
      ))
  nil
  )

(defun object-code-file (base-file-name)
  (concatenate 'string base-file-name *binary-lisp-extension*)
 )

(defun make 
    
    (system-symbol forms filelist load-all compile-any compile-all verbose dry-run)
  
  ;; Consider each file in turn.  If it needs to be compiled
  ;; and COMPILE-ANY is T, or COMPILE-ALL is non-nil, 
  ;; compile it and then load it.
  
  ;; If it does not need to be compiled, load it if LOAD-ALL is T
  ;; or it has not already been loaded. 
  
  ;; Evaluate the (optional) form paired with each file before compiling
  ;; the file.
  
  (when (and (null compile-any) compile-all)
    (errmsg "~%Incompatible options to make.  COMPILE-ANY is NIL while COMPILE-ALL is T.~%")
    (return-from make nil)
    )
  
  (if (or (null (listp filelist))
          (null (every #'stringp filelist)))
      (progn
        (errmsg "~%Make expects a list of strings for each file component.")
        (errmsg "Actual file component argument: ~S~%" filelist)
        (return-from make nil)
        ))
  
  (flet
      ((source-loaded? 
	   (source-file)
	 (member source-file 
		 (get system-symbol :loaded-defsystem-files)
		 :test #'string=
		 ))
       (load-source-or-object
	   (source-file object-file)
	 (let ((file-to-load (or object-file source-file)))
	   (unless (and (not dry-run) (null verbose))
	     (fresh-line) 
	     (format t ";; Make:  loading file ~S~%" file-to-load) 
	     (force-output t))
	   (unless dry-run (load file-to-load))
	   ))
       (record-load 
	   (source-file)
	 (pushnew source-file (get system-symbol :loaded-defsystem-files) :test #'string=)
	 )
       (file-needs-compiling? 
	   (source-file object-file)
	 (or (null (probe-file object-file))
	     (> (file-write-date source-file)
		(file-write-date object-file)
		)))
       (compile-source 
	   (source-file object-file)
	 (unless (and (not dry-run) (null verbose))
	   (fresh-line) 
	   (format t ";; Make:  compiling file ~S~%" source-file) 
	   (force-output t))
	 (unless dry-run (compile-file source-file :output-file object-file))
	 )
       
       )
    
    (let* ((*load-verbose* (if (eq verbose t) t nil))
           (*load-print* *load-verbose*)
           (*compile-verbose* *load-verbose*)
           (*compile-print* *load-verbose*)
           )
      
      (with-compilation-unit ()
        
       (mapc
            
	#'(lambda (form file)
                
	    (when (and form (or dry-run (not (null verbose))))
	      (fresh-line) (format t ";; Make:  evaluating ~S~%" form)
	      )
	    (unless dry-run (eval form))
                
	    (let ((source-file (source-code-file file))
		  (object-file (object-code-file file))
		  (changed nil)
		  )
                  
	      (when (null (probe-file source-file))
		(errmsg "~%File ~S does not exist~%" file)
		(errmsg "Aborting make...~%")
		(return-from make nil)
		)
                  
	      ;; If not compiling, load the source file if necessary.
                  
	      (when (not compile-any)
		(when (or load-all (not (source-loaded? source-file)))
		  (load-source-or-object source-file nil)
		  (record-load source-file)
		  ))
                  
	      (when compile-any
		(when (or compile-all (file-needs-compiling? source-file object-file))
		  (compile-source source-file object-file)
		  (setq changed t)
		  )
		(when (or load-all changed (not (source-loaded? source-file)))
		  (load-source-or-object source-file object-file)
		  (record-load source-file)
		  ))
                  
	      ))
          
	forms
	filelist
          
	))
      
      t
      
      )))


(defun find-defsystem (system-name)

  ;; given the name of a system to make, figure out where
  ;; the file describing how to make the system is located.

  (mapc
    #'(lambda (searchpath)
	(let ((pathname 
		(concatenate 'string 
                 searchpath system-name *defsystem-suffix*)))
    (format t pathname)
    (print (probe-file pathname))
	  (when (probe-file pathname)
	    (return-from find-defsystem pathname)
	   )
	 ))
    *defsystem-search-path*
   )
  nil
 )


(defun check-file-specification (file-spec)
  (if (or (null (consp file-spec))
	  (not (every #'stringp file-spec)))
      (errmsg "MAKE-SYSTEM-FILE-LIST: Bad file specification: ~S" file-spec)
    (combine-string-list-to-form-full-file-path file-spec)
   ))


;;; The user puts into his .sys file a call to this function,
;;; giving as arguments lists of strings which define logical
;;; pathnames for his various files.  Previously in his .sys file
;;; the user has defined aliases to be used for logical pathnames
;;; (see above)

(defun make-system-file-list (system-symbol &rest compile-specifications)
  (dolist (c compile-specifications)
    (when (null (consp c))
      (error "One of the compile specifications is not a list: ~S" c)
     ))
  (let* ((forms-to-evaluate
           (mapcar #'(lambda (x) (if (stringp (car x)) nil (car x)))
                   compile-specifications))
         (file-specifications
           (mapcar #'(lambda (x) (if (stringp (car x)) x (cadr x)))
                   compile-specifications))
         (defsystem-files
           (mapcar #'check-file-specification file-specifications))
        )
    (setf (get system-symbol :defsystem-forms) forms-to-evaluate)
    (setf (get system-symbol :defsystem-files-list) defsystem-files)
    nil
   ))


(defun make-system (system-name load-all compile-any compile-all verbose dry-run)
       
  ;; locate and load the system definition file

  (when (not (stringp system-name))
    (if (symbolp system-name) 
	(setq system-name (symbol-name system-name))
      (progn
	(errmsg "Argument to make-system is not a string: ~S~%" system-name)
	(return-from make-system nil)
	)))
       
  (let ((full-system-pathname (find-defsystem system-name))
        (system-symbol (intern system-name (find-package :keyword)))
        )

    (when (null full-system-pathname)
      (errmsg "Cannot find defsystem ~S~%" system-name)
      (return-from make-system nil)
      )

    (when (or dry-run (not (null verbose)))
      (format t "~%;; Defsystem: Loading system definition: ~A" full-system-pathname))

    (load full-system-pathname)

    (make system-symbol 
          (get system-symbol :defsystem-forms)
          (get system-symbol :defsystem-files-list)
          load-all compile-any compile-all verbose dry-run 
          )

    (terpri)

    ))

;;; Full COMPILE-LOAD 

(defun msc (name &key (verbose :normal) (dry-run nil))
  (make-system name t t t verbose dry-run)
  )

;;; Standard COMPILE-LOAD (only files that have changed)

(defun ms (name &key (verbose :normal) (dry-run nil))
  (make-system name nil t nil verbose dry-run)
  )

;;; Standard load of changed source files 

(defun mss (name &key (verbose :normal) (dry-run nil))
  (make-system name nil nil nil verbose dry-run)
  )

;;; Load of all source files

(defun mss-all (name &key (verbose :normal) (dry-run nil))
  (make-system name t nil nil verbose dry-run)
  )
