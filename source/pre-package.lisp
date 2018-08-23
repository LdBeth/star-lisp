;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: *SIM-I; Base: 10 -*-


(in-package #+LISPWORKS :user #-LISPWORKS :common-lisp-user)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defmacro defpackage-with-lists-symbols
    (name (&key (shadow-list-symbol nil)
                (shadowing-import-from-package nil)
                (shadowing-import-from-list-symbol nil)
                (use-list-symbol nil)
                (import-from-package nil)
                (import-from-list-symbol nil)
                (export-list-symbol nil)
                )
          &rest other-defpackage-options
          )
  `(defpackage ,name
     ,@(when shadow-list-symbol `((:shadow ,@(eval shadow-list-symbol))))
     ,@(when shadowing-import-from-list-symbol 
         `((:shadowing-import-from 
            ,(eval shadowing-import-from-package) 
            ,@(eval shadowing-import-from-list-symbol)
            )))
     ,@(when use-list-symbol `((:use ,@(eval use-list-symbol))))
     ,@(when import-from-list-symbol 
         `((:import-from 
            ,(eval import-from-package)
            ,@(eval import-from-list-symbol)
            )))
     ,@(when export-list-symbol `((:export ,@(eval export-list-symbol))))
     ,@other-defpackage-options
     ))


(defparameter *common-lisp-shadowed-symbol-names*
  '("PROCLAIM" "DECLAIM" "DEFTYPE")
  )

