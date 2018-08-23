;;; -*- MODE: LISP; SYNTAX: COMMON-LISP; PACKAGE: *SIM-I; BASE: 10; MUSER: YES; -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;>
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defparameter *help-string*
 "
         Release Notes for 5.1 and 5.2 *Lisp exist.  They should be added to your
         existing 5.0 *Lisp documentation.

         New with 5.1 *Lisp is the *Lisp Library,
         including routines which do FFT and matrix multiply.
         Documentation for the *Lisp library exists at TMC in
         /cm/starlisp/library/f5201/documentation.text.
         Ask your applications engineer of systems administrator
         where this file is at your site.

         New with 5.2 *Lisp is *Graphics, a system for easily displaying
         pvar data to a CM Framebuffer, and X Window or a Symbolics Screen.
         The *Graphics Manual can be found with the Paris documentation
         about the CM Framebuffer and the Paris Rendering and Generic
         Display facilities.

         You can find out some information about most Starlisp symbols by typing
         (help 'symbol), e.g., (help '+!!)

         Thinking Machines Corporation offers a training course which covers
         Starlisp and other aspects of Connection Machine programming.

         The source to Starlisp is available on line.  On a Symbolics Lisp Machine,
         use Meta-. to examine source files.  In a Unix environment, ask your
         systems administrator or Applications Engineer where the Starlisp source
         files were installed.  If you have the GMACS editor and TMC's enhancements
         to it then you can use Meta-. from within GMACS, along with many other
         useful debugging and programming tools.

         The *Lisp Simulator, which runs on Machintoshes, Lisp Machines
         Unix boxes, VMS and various other platforms that have Common
         Lisp available is available from TMC Customer Support.

         There are example programs in the directory
         /cm/starlisp/interpreter/f5201/ at TMC and/or where the sources
         for the Starlisp Simulator are with names that have the text 'example' in them.
         Ask your applications engineer or systems manager exactly where
         this directory lives at your installation."

	)


(defvar *do-not-know-string*
	"~%The symbol ~S is not a documented, exported, Starlisp symbol.~
       There is no information about it inside Starlisp.~%"
  )

(defun find-value-for-keyword (keyword list)
  (let ((position (position keyword list :test #'eq)))
    (if (null position)
	nil
	(nth (1+ position) list)
	)))


(defun parse-doc-code-string (doc-code-string)
  (if (null doc-code-string)
      (values nil nil)
      (let ((semipos (position #\; doc-code-string)))
	(if semipos
	    (multiple-value-bind (document page)
		(parse-single-doc-code-string (subseq doc-code-string 0 semipos))
	      (multiple-value-bind (documents pages)
		  (parse-doc-code-string (subseq doc-code-string (1+ semipos)))
		(values (cons document documents) (cons page pages))
		))
	    (multiple-value-bind (document page)
		(parse-single-doc-code-string doc-code-string)
	      (values (list document) (list page))
	      )))))


(defun parse-single-doc-code-string (doc-code-string)
  (let* ((document-code-string (subseq doc-code-string 0 2))
	 (document-code-symbol (intern-in-starlisp-internal-package document-code-string))
	 (document-text (cadr (assoc document-code-symbol *doc-code-to-english-mapping*)))
	 )
    (if (eql (length doc-code-string) 2)
	(values document-text nil)
	(if (not (eql #\, (char doc-code-string 2)))
	    (values document-text nil)
	    (values document-text (subseq doc-code-string 3))
	    ))))


(defun help (&optional symbol)

  (when (not *load-starlisp-interface-info*)
    (format t "~%Sorry, Starlisp Help System not loaded.  No help available.")
    (return-from help nil)
    )

  (when (stringp symbol) (setq symbol (intern-in-starlisp-package symbol)))
  (when (not (symbolp symbol))
    (error "Sorry, I can only help you if you give me the name of something.  ~A is not a name" symbol)
    )
  (if (null symbol)
      (format t "~%~%~A~%" *help-string*)
      (let ((*print-case* :downcase))
	(if (null (member symbol *all-external-symbols*))
	    (format t *do-not-know-string* symbol)
	    (let ((descriptor (get symbol :starlisp-descriptor)))
	      (if (or (null descriptor) (not (stringp descriptor)))
		  (error "Internal error.  The symbol ~S in on the *all-external-symbols* list, but has no descriptor"
			 symbol
			 )
		  (let* ((descriptor (read-from-string descriptor))
			 (type (find-value-for-keyword :type descriptor))
			 (arglist (find-value-for-keyword :arglist descriptor))
			 (docptr (find-value-for-keyword :docptr descriptor))
			 (info (find-value-for-keyword :info descriptor))
			 )
		    (when (stringp arglist) (setq arglist (read-from-string arglist nil nil)))
		    (format t "~%The Starlisp symbol ~A is a ~A." symbol (intern (symbol-name type)))
		    (when arglist
		      (format t "~%  ~A has an argument list of the form ~S." symbol arglist)
		      )
		    (when docptr
		      (multiple-value-bind (documents pages)
			  (parse-doc-code-string docptr)
			(mapcar
			  #'(lambda (document page)
			      (format t "~%  Documentation for ~A exists in the ~A" symbol document)
			      (if page
				  (format t ", p. ~A." page)
				  (format t ".")
				  ))
			  documents
			  pages
			  )))
		    (when info
		      (map nil
			   #'(lambda (char)
			       (let ((english
				       (cadr (assoc (intern-in-starlisp-internal-package (string char))
						    *character-to-english-mapping*
						    ))))
				 (format t "~%  The ~A ~A ~A" (intern (symbol-name type)) symbol english)
				 ))
			   info
			   ))
		    (terpri)
		    )))))))
