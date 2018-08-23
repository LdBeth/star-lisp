;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Muser: yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;; *****     WARNING WARNING WARNING WARNING WARNING WARNING      *****
;;;;
;;;; This code is shared between the *Lisp Interpreter and the
;;;; *Lisp Simulator.  DO NOT MAKE CHANGES IN THIS CODE UNLESS
;;;; YOU ARE ABSOLUTELY SURE THE CHANGES APPLY EQUALLY TO BOTH
;;;; SYSTEMS OR YOU ARE VERY CAREFUL TO CONDITIONALLY COMPILE!
;;;; VIOLATE THIS WARNING AT YOUR OWN RISK!
;;;;
;;;; *****     WARNING WARNING WARNING WARNING WARNING WARNING      *****


;;; This file contains hardware/simulator independent code implementing
;;; the debugging functions ppp, ppp-css and other miscellaneous useful
;;; printing functions.


(defvar *value-to-print-when-error-found* 'error)


(defun pref-or-junk (pvar processor)
  (progn
    ;;(with-compile-time-local-property (compile-time-prop *compilep* nil)
    (with-all-errors-trapped 
        (pref pvar processor)
      *value-to-print-when-error-found*)))

(defun pref-grid-or-junk (pvar &rest addresses)
  (pref-or-junk pvar (apply 'cube-from-grid-address addresses))
  )

(defmacro ppp (pvar &rest keyargs)
  `(with-css-saved (ppp-internal ,pvar ,@keyargs)))

(defmacro ppp!! (pvar &rest keyargs)
  ;; this can not use with-css-saved, because it deallocates everything, including
  ;; the argument to ppp!!, if it is on the stack
  `(ppp-internal ,pvar ,@keyargs :return-argument-pvar t))

(defmacro pppdbg (pvar &rest keyargs)
  `(ppp ,pvar :title (format nil "~A" ',pvar) ,@keyargs))


(defmacro pretty-print-pvar (&rest args) `(ppp ,@args))


;; pretty print all the component values of a pvar.
;; The user can specify a format for printing, whether
;; to print in grid or cube order, and how many values
;; to print per line.  If the user is using grid
;; addressing he may also specify in which order
;; the dimensions are to be printed out
;; (for > 2 dimensions).  If the user is using cube
;; addressing he can specify a range of processors to
;; be printed out.

(defvar *ppp-mode-for-2d* :cube)

(defun ppp-internal
    
    (pvar
     &key
     (mode *ppp-default-mode* mode-provided)
     (format *ppp-default-format*)
     (per-line *ppp-default-per-line*)
     (title *ppp-default-title*)
     (start *ppp-default-start* start-provided)
     (end *ppp-default-end* end-provided)
     (processor-list *ppp-default-processor-list*)
     (ordering *ppp-default-ordering*)
     (print-arrays t)
     (return-argument-pvar nil)
     ;; ((:pretty *print-pretty*) nil)
     stream)
  
  (simple-pvar-argument!! pvar)
  
  (let* ((*print-array* print-arrays)
         (current-vp-set *current-vp-set*)
         (*standard-output* 
          (cond ((eq stream nil) *standard-output*)
                ((eq stream t) *terminal-io*)
                ((streamp stream) stream)
                (t (error "Invalid stream argument ~S to ppp." stream)))))
    
    (flet ((argument-error-test 
            (ok? format-string &rest format-args)
            (when (not ok?)
              (apply #'format *error-output* format-string format-args)
              (return-from ppp-internal (if return-argument-pvar pvar nil))
              ))
           (integer-range-test (x low high) (and (integerp x) (>= x low) (< x high)))
           )
      
      (fresh-line *standard-output*)
      
      ;; check all the arguments.
      
      (argument-error-test (pvarp pvar) "Not a pvar: ~A~%" pvar)
      
      (*with-vp-set 
       (pvar-vp-set pvar)
       
       ;; This code makes no sense to me now.  JP.
       ;; I think this says that if the pvar being printed is in another Vp Set,
       ;; and the user specified an end which is bigger than *ppp-default-end*
       ;; (which presumably is *number-of-processors-limit* for that Vp Set)
       ;; then smash the user-provided end value to be OK.  But shouldn't it
       ;; just error out if end > *number-of-processors-limit*, and just be
       ;; ok if *ppp-default-end* < end < *number-of-processors-limit* ?
       
       (when (and (not (eq *current-vp-set* current-vp-set))
                  (and (integerp end) (integerp *ppp-default-end*) (> end *ppp-default-end*))
                  (setq end *ppp-default-end*)
                  ))
       
       ;; When an END argument was not provided, and the pvar we are
       ;; printing is in a different Vp Set, then if *ppp-default-end*
       ;; for that Vp Set is reasonable, make end have that value.
       
       (when (and (not (eq *current-vp-set* current-vp-set))
                  (not end-provided)
                  (integerp *ppp-default-end*)
                  (setq end *ppp-default-end*)
                  ))
       
       (argument-error-test (or (eq mode :cube) (eq mode :grid)) "Bad mode keyword: ~A~%" mode)
       
       (multiple-value-bind (start end mode error? error-string)
           
           (determine-reasonable-values-for-start-and-end mode mode-provided start end start-provided end-provided)
         
         (when error? (argument-error-test nil error-string))
         
         (cond
          ((eq mode :cube)
           (argument-error-test
            (integer-range-test start 0 *number-of-processors-limit*)
            "Invalid start keyword value: ~A~%"
            start
            )
           (argument-error-test
            (integer-range-test end (1+ start) (1+ *number-of-processors-limit*))
            "Invalid end keyword value: ~A~%"
            end
            ))
          ((eq mode :grid)
           (argument-error-test
            (eql (length start) *number-of-dimensions*)
            "There are ~D dimensions in the current VP SET, but you provided ~D coordinates for :start"
            *number-of-dimensions* (length start)
            )
           (argument-error-test
            (eql (length end) *number-of-dimensions*)
            "There are ~D dimensions in the current VP SET, but you provided ~D coordinates for :end"
            *number-of-dimensions* (length end)
            )
           (flet ((check-grid-coordinate 
                   (x low limit keyname dimension decr)
                   (argument-error-test		
                    (integer-range-test x low limit)
                    "For ~S, grid coordinate ~D, which has value ~S, is not between ~D and ~D"
                    keyname dimension x (- low decr) (- limit decr)
                    )))
             (let ((count -1))
               (mapc #'(lambda (start limit) (check-grid-coordinate start 0 limit :start (incf count) 0))
                 start *current-cm-configuration*
                 ))
             (let ((count -1))
               (mapc #'(lambda (start end limit)
                         (check-grid-coordinate end (1+ start) (1+ limit) :end (incf count) 1)
                         )
                 start end *current-cm-configuration*
                 ))
             ))
          )
         
         (argument-error-test (stringp format) "Format keyword value is not a string: ~S~%" format)
         (argument-error-test
          (or (null per-line) (integer-range-test per-line 1 1000000))
          "Per-line keyword value ~A is not a small integer~%"
          per-line
          )
         (argument-error-test (or (null title) (symbolp title) (stringp title)) "Title keyword value not a string: ~A~%" title)
         
         ;; If there is an ordering given, we assume grid mode.
         ;; The ordering is just a list of dimensions
         ;; The dimensions are numbered from 0 up to (1- *number-of-dimensions*)
         ;; which is the default dimension ordering.
         ;; The ordering controls the iteration order over dimensions.
         ;; we ignore ordering for 2-dimensions, making our life easier.
         
         (when ordering
           (argument-error-test
            (eq mode :grid)
            "You specified a dimensions ordering, but you also specified or defaulted to :CUBE mode"
            ))
         
         (let ((default-ordering nil))
           (dotimes (j *number-of-dimensions*) (push j default-ordering))
           (setq default-ordering (nreverse default-ordering))
           (if (null ordering)
               (setq ordering default-ordering)
             (argument-error-test
              (equal default-ordering (sort (copy-list ordering) #'<))
              "Bad ordering ~S. ~
                     An ordering must be a list of non-repeated integers from 0 to *number-of-dimensions*"
              ordering
              )))
         
         (argument-error-test
          (every #'(lambda (n) (integer-range-test n start end)) processor-list)
          "Processor list contains invalid processor number: ~A~%" processor-list
          )
         
         ;; start printing!
         
         (and title (if per-line (format *standard-output* "~A:~%" title) (format *standard-output* "~A: " title)))
         
         ;; print using cube addressing
         
         (cond
          ((eq mode :cube)
           (ppp-cube-ordering pvar start end format per-line processor-list)
           )
          ((and (eq mode :grid) (eql 1 *number-of-dimensions*))
           (ppp-1d-news pvar start end format per-line)
           )
          ((and (eq mode :grid) (eql 2 *number-of-dimensions*))
           (ppp-2d-news pvar start end format per-line ordering)
           )
          (t
           (print-hypergrid pvar ordering start end format per-line)
           ))
         
         ))))
  
  (if return-argument-pvar pvar (values))
  
  )


(defun determine-reasonable-values-for-start-and-end (mode mode-provided start end start-provided end-provided)
  
  (let ((error-string nil))
    
    (cond
     
     ((eql 1 *number-of-dimensions*)
      (cond
       (mode-provided
        (cond
         ((eq mode :grid)
          (if (not (listp start)) (setq start (list start)))
          (if (not (listp end)) (setq end (list end)))
          )
         ((eq mode :cube)
          (when (listp start)
            (setq error-string "You specified :CUBE mode but :START was a list instead of a cube address")
            )
          (when (listp end)
            (setq error-string "You specified :CUBE mode but :END was a list instead of a cube address")
            ))))
       ((not mode-provided)
        (cond
         ((eq mode :grid)
          (if (not (listp start)) (setq start (list start)))
          (if (not (listp end)) (setq end (list end)))
          )
         ((eq mode :cube)
          (when (listp start)
            (setq error-string "You defaulted to :CUBE mode but :START was a list instead of a cube address")
            )
          (when (listp end)
            (setq error-string "You defaulted to :CUBE mode but :END was a list instead of a cube address")
            )))
        )))
     
     (t
      (cond
       ((eq mode :grid)
        (cond
         ((and (listp start) (listp end)))
         ((and (listp start) (not (listp end)))
          (cond
           (end-provided
            (setq error-string (format nil "You specified :GRID mode but :END, ~S, is not a list" end))
            )
           (t (setq end (copy-list *current-cm-configuration*)))
           ))
         ((and (not (listp start)) (listp end))
          (cond
           (start-provided
            (setq error-string (format nil "You specified :GRID mode but :START, ~S, is not a list" start))
            )
           (t (setq start (make-list *number-of-dimensions* :initial-element 0)))
           ))
         ((and (not (listp start)) (not (listp end)))
          (cond
           ((or mode-provided (eq *ppp-default-mode* :grid))
            (cond
             (start-provided
              (setq error-string (format nil "You specified :GRID mode but :START, ~S, is not a list" start))
              )
             (end-provided
              (setq error-string (format nil "You specified :GRID mode but :END, ~S, is not a list" end))
              )
             (t
              (setq start (make-list *number-of-dimensions* :initial-element 0))
              (setq end (copy-list *current-cm-configuration*))
              )))
           (t (setq mode :cube))
           ))
         (t (error "This is impossible"))
         ))
       ((eq mode :cube)
        (cond
         ((and (not (listp start)) (not (listp end))))
         ((and (listp start) (not (listp end)))
          (cond
           ((or end-provided mode-provided)
            (setq error-string (format nil "You specified :CUBE mode but :START, ~S, is a list" start))
            )
           (t
            (setq end (copy-list *current-cm-configuration*))
            (setq mode :grid)
            )))
         ((and (not (listp start)) (listp end))
          (cond
           ((or start-provided mode-provided)
            (setq error-string (format nil "You specified :CUBE mode but :END, ~S, is a list" end))
            )
           (t
            (setq start (make-list *number-of-dimensions* :initial-element 0))
            (setq mode :grid)
            )))
         ((and (listp start) (listp end))
          (cond
           ((and start-provided end-provided (not mode-provided)) (setq mode :grid))
           ((and mode-provided (not start-provided) (not end-provided))
            (setq start 0)
            (setq end *number-of-processors-limit*)
            )
           (t (setq error-string
                    (format nil "You specified :CUBE mode but :START, ~S, and :END, ~S, were both lists") start end
                  ))))
         (t (error "This is impossible"))
         ))))
     
     )
    
    (values start end mode error-string error-string)
    
    ))


(defvar *ppp-look-at-output* t)
(defvar *ppp-too-much-for-one-line* 50)

(defun ppp-format (destination control-string &rest args)
  (if *ppp-look-at-output*
      (let ((result (apply 'format nil control-string args)))
	(write-string result destination)
	(if (or (find #\Newline result) (> (length result) *ppp-too-much-for-one-line*))
	    (terpri destination)))
      (apply 'format destination control-string args)))

(defun ppp-cube-ordering (pvar start end format per-line processor-list)
  (let ((print-it nil) (count 0))
    (do ((j start (1+ j)))
	((>= j end))
      (setq print-it (if processor-list (member j processor-list) t))
      (when print-it
	(ppp-format *standard-output* format (pref-or-junk pvar j))
	(when per-line
	  (when (eql (incf count) per-line)
	    (setq count 0)
	    (terpri *standard-output*)))))))
      

(defun ppp-1d-news (pvar start end format per-line)
  (let ((count 0) (start (car start)) (end (car end)))
    (do ((j start (1+ j)))
	((>= j end))
      (ppp-format *standard-output* format (pref-grid-or-junk pvar j))
      (when per-line
	(when (eql (incf count) per-line)
	  (setq count 0)
	  (terpri *standard-output*))))))
      


(defun ppp-2d-news (pvar start end format per-line ordering)
  (let ((count 0))
    (flet
      ((print-value (x y)
	 (ppp-format *standard-output* format (pref-grid-or-junk pvar x y))
	 (when per-line
	   (when (eql (incf count) per-line)
	     (setq count 0)
	     (terpri *standard-output*)
	     ))))
      (if (equal ordering '(0 1))
	  (progn
	    (format *standard-output* "~%     DIMENSION 0 (X)  ----->~%~%")
	    (do ((y (nth 1 start) (1+ y)))
		((= y (nth 1 end)))
	      (setq count 0)
	      (do ((x (nth 0 start) (1+ x)))
		  ((= x (nth 0 end)))
		(print-value x y)
		)
	      (terpri *standard-output*)
	      ))
	  (progn
	    (format *standard-output* "~%     DIMENSION 1 (Y)  ----->~%~%")
	    (do ((x (nth 0 start) (1+ x)))
		((= x (nth 0 end)))
	      (setq count 0)
	      (do ((y (nth 1 start) (1+ y)))
		  ((= y (nth 1 end)))
		(print-value x y)
		)
	      (terpri *standard-output*)
	      ))))))

;;; auxiliary routines to display pvar values in more than
;;; 2 dimensions.  2-dimensional slices are printed out
;;; successively, with the coordinates of the slice noted
;;; above the grid.

(defun print-hypergrid (pvar ordering start end format per-line)

  (print ordering)

  ;; Get the last two dimensions.  These two dimensions
  ;; are the slice that we print.  The other dimensions
  ;; we iterate over.

  (let* ((ordering-reversed (reverse ordering))
	 (last-dimension (pop ordering-reversed))
	 (second-to-last-dimension (pop ordering-reversed))
	 (dimension-ordering nil))
    (dolist (dimension ordering-reversed)
      (push (list dimension nil) dimension-ordering)
      )
    (print-hypergrid-aux
      0
      dimension-ordering
      second-to-last-dimension
      last-dimension
      pvar
      start
      end
      format
      per-line
      )))


(defun print-hypergrid-aux
       (current-dimension-index dimension-ordering second-to-last-dimension last-dimension pvar start end format per-line)

  (let* ((current-dimension-info (nth current-dimension-index dimension-ordering))
	 (current-dimension (car current-dimension-info))
	 )

    (do ((j (nth current-dimension start) (1+ j)))
	((>= j (nth current-dimension end)))

      (setf (cadr current-dimension-info) j)

      ;; If this is our last dimension before getting to our
      ;; 2d slice, call the print slice routine, otherwise
      ;; keep recursing downward in dimensions.

      (if (eql current-dimension-index (1- (length dimension-ordering)))

	  (print-subgrid
	    pvar
	    format
	    second-to-last-dimension
	    last-dimension
	    dimension-ordering
	    start
	    end
	    per-line
	    )
	    
	  (print-hypergrid-aux
	    (1+ current-dimension-index)
	    dimension-ordering
	    second-to-last-dimension
	    last-dimension
	    pvar
	    start
	    end
	    format
	    per-line
	    )))))


(defun print-subgrid (pvar format first-dimension second-dimension other-dimension-ordering start end per-line)
  (terpri *standard-output*)
  (terpri *standard-output*)
  (let ((coordinate-list (make-list *number-of-dimensions*)))
    (dolist (dimension-info other-dimension-ordering)
      (let ((dimension (car dimension-info))
	    (coordinate (cadr dimension-info))
	    )
	(setf (nth dimension coordinate-list) coordinate)
	(format *standard-output* "DIMENSION ~S, COORDINATE ~S~%" dimension coordinate)
	))
    (format *standard-output* "~%     DIMENSION ~S    ----->~%~%" first-dimension)
    (do ((second (nth second-dimension start) (1+ second)))
	((>= second (nth second-dimension end)))
      (setf (nth second-dimension coordinate-list) second)
      (let ((count 0))
	(do ((first (nth first-dimension start) (1+ first)))
	    ((>= first (nth first-dimension end)))
	  (setf (nth first-dimension coordinate-list) first)
	  (ppp-format *standard-output* format (apply #'pref-grid-or-junk pvar coordinate-list))
	  (when per-line
	    (when (eql (incf count) per-line)
	      (setq count 0)
	      (terpri *standard-output*)))))
      (terpri *standard-output*))))


;; Pretty print out the active processors' cube addresses
(defun display-active-processors ()
  (terpri *standard-output*)
  (when (*and nil!!)
    (format *standard-output* "No active processors...~%")
    (return-from display-active-processors nil))
  (let ((active-processors (vector-of-active-processors))
        (count 0))
    (dotimes (i (length active-processors))
      (format *standard-output* "~5S " (aref active-processors i))
      (incf count)
      (when (eql count 12) (setq count 0) (terpri *standard-output*))))
  (values))


#+*lisp-simulator
(defun vector-of-active-processors (&optional max)
  (*let (compressed-addresses)
    (declare (type (field-pvar *current-send-address-length*) compressed-addresses))
    (let ((number-active (*sum (!! 1))))
      (if (zerop number-active) (return-from vector-of-active-processors '#()))
      (*pset :no-collisions (self-address!!) compressed-addresses (enumerate!!))
      (if max (setq number-active (min number-active max)))
      (let ((result-vector
	      (make-array number-active :element-type `(unsigned-byte ,*current-send-address-length*))
	      ))
	(pvar-to-array compressed-addresses result-vector :cube-address-end number-active)
	result-vector
	))))

(defun list-of-active-processors () 
  (concatenate 'list (vector-of-active-processors)))

(defmacro loap () '(list-of-active-processors))


(defun list-of-grid-coordinates (list-of-active-processors)
  (mapcar
    #'(lambda (cube-address)
	(let ((grid-addresses nil))
	  (dotimes (j *number-of-dimensions*)
	    (push (grid-from-cube-address cube-address j) grid-addresses)
	    )
	  (setq grid-addresses (nreverse grid-addresses))
	  grid-addresses
	  ))
    list-of-active-processors
    ))

(defun sorted-list-of-grid-coordinates (list-of-grid-coordinates)
  (labels
    ((list-less-than (x y)
       (cond
	 ((null x) nil)
	 ((< (car x) (car y)) t)
	 ((> (car x) (car y)) nil)
	 (t (list-less-than (cdr x) (cdr y)))
	 )))
    (setq list-of-grid-coordinates (sort list-of-grid-coordinates #'list-less-than))
    list-of-grid-coordinates
    ))

;; Pretty print active processor cube addresses
;; and contents of a pvar in those processors

(defun display-active-processor-values

       (
	pvar
	&key
	(format "~S ")
	(start 0)
	(end *number-of-processors-limit*)
	title
	(mode :cube)
	)
  (declare (ignore mode))
  (when (not (stringp format))
    (error "Format key value is not a string: ~S" format))
  (when (or (not (integerp start))
	    (not (integerp end))
	    (< end start)
	    (< start 0)
	    (> end *number-of-processors-limit*))
    (error "Start or End keyword value invalid"))
  (terpri *standard-output*)
  (when (not (*or t!!))
    (format *standard-output* "No active processors...~%")
    (return-from display-active-processor-values nil))
  (if (stringp title) (format *standard-output* "~A: " title))
  (let ((active-processors (vector-of-active-processors))
	(count 0)
	(format-string (format nil "~A: ~A " "~5@S" format)))
    (dotimes (i (length active-processors))
      (let ((p (aref active-processors i)))
	(when (and (>= p start) (< p end))
	  (ppp-format *standard-output* format-string p (pref-or-junk pvar p))
	  (incf count)
	  (when (eql count 8) (setq count 0) (terpri *standard-output*))))))
  (values))


(defmacro pretty-print-pvar-in-currently-selected-set (&rest args)
  `(*let () (display-active-processor-values ,@args)))
(defmacro ppp-css (&rest args)
  #+symbolics (declare (si:arglist pvar &key format start end title))
  #+lucid (declare (sys::arglist (pvar &key format start end title)))
  `(*let () (display-active-processor-values ,@args)))


(defun-wco ppp-address-object

	   (address-object-pvar
	     &key title (start 0 start-provided) (end *number-of-processors-limit* end-provided) (mode :cube))

  (simple-pvar-argument!! address-object-pvar)
  
  (when (and (null start-provided) (eq mode :grid)) (setq start (make-list *number-of-dimensions* :initial-element 0)))
  (when (and (null end-provided) (eq mode :grid)) (setq end (copy-list *current-cm-configuration*)))

  (terpri *standard-output*)
  (when title (format *standard-output* "~A:~%" title))

  (let ((geometry-id (address-object-cached-geometry-id address-object-pvar)))

    ;; Figure out the rank in each processor.
    ;; Use -1 if we have *illegal-geometry-id*.
    ;; For printing, we will print out NIL for the rank if *illegal-geometry-id*.

    (*let (rank printing-rank)
      (declare (type (pvar (signed-byte 32)) rank))
      (declare (type (pvar t) printing-rank))
      (*all
	(*map-geometries
	  #'(lambda (id)
	      (*set rank
		    (if (eql id *illegal-geometry-id*)
			(!! -1)
			(!! (geometry-rank (geometry-from-id id)))
			)))
	  address-object-pvar
	  )
	(*set printing-rank (if!! (=!! (!! -1) rank) nil!! rank))
	)

      ;; If the geometry id is cached, just print out the
      ;; scalar id and rank.  Otherwise print them out
      ;; in each processor.

      (if geometry-id
	  (format *standard-output* "Single cached geometry id: ~D, Rank: ~D" geometry-id (geometry-rank (geometry-from-id geometry-id)))
	  (progn
	    (ppp (alias!! (address-object-geometry-id!! address-object-pvar))
		 :title "Geometry Id" :start start :end end :mode mode
		 )
	    (ppp printing-rank :title "Address Rank" :start start :end end :mode mode)
	    ))

      ;; Print out the cube address.  This should always be 0 for
      ;; processors with *illegal-geometry-id*.

      (ppp (alias!! (address-object-cube-address!! address-object-pvar))
	   :title "Cube Address     " :start start :end end :mode mode
	   )

      ;; Print out the grid coordinates.
      ;; Print out NIL for *illegal-geometry-id* and also
      ;; for those geometry id's which do not have an nth
      ;; grid address.  (i.e., if there are two different
      ;; geometries, and one is 2d and one is 3d, then
      ;; print out NIL in the 2d ones when printing out
      ;; the third grid coordinate of the 3d one).

      (*all
	(let ((max-rank (*max rank)))
	  (*let (grid-address)
	    (declare (type (pvar t) grid-address))
	    (dotimes (j max-rank)
	      (*all (*set grid-address nil!!))
	      (*when (and!! (/=!! (!! -1) rank) (>!! rank (!! j)))
		(*set grid-address (address-nth!! address-object-pvar (!! j)))
		)
	      (ppp grid-address :title (format nil "Grid Coordinate ~D" j) :start start :end end :mode mode)
	      ))))

      )))


(defun ppp-struct (pvar per-line &key (start nil) (end nil) (print-array t) (stream t) (width 8) (title t))

  ;; Pvar:        The structure pvar to be pretty printed.
  ;; Per-line:    The number of processors shown per line.  MUST be provided.
  ;; Start:       Beginning processor index.
  ;; End:         Ending processor index.
  ;; Print-array: Whether arrays are printed out showing their elements.
  ;; Stream:      Where the output goes.
  ;; Width:       The width of each field being printed out.  Defaults to 8.
  ;; Title:       If null, no title.  By default, the name of the *DEFSTRUCT.

  (simple-pvar-argument!! pvar)
  
  (new-pvar-check pvar 'ppp-struct)
  (assert (eq :structure (pvar-type pvar)) ()
	  "The pvar given to PPP-STRUCT, ~S, is not a structure pvar"
	  pvar
	  )
  (assert (and (integerp width) (plusp width)) () "Width must be a positive integer")
  (assert (and (integerp per-line) (plusp per-line)) () "Per-line must be a positive integer")

  (let ((*print-array* print-array)
	(*pvar* pvar)
	(format-string (format nil "~~~DS " width))
	)
    (declare (special *print-array* *pvar*))

    (*with-vp-set (pvar-vp-set pvar)

      (setq start (if (null start) 0 start))
      (setq end (if (null end) *number-of-processors-limit* end))

      (assert (and (integerp start)
		   (integerp end)
		   (not (minusp start))
		   (< end (1+ *number-of-processors-limit*))
		   (< start end)
		   )
	      ()
	      "Start, ~S, and End, ~D, are not reasonable parameters"
	      start end
	      )

      (*let ()
	(declare (return-pvar-p nil))

	(let ((list-of-aliased-slot-pvars nil)
	      (list-of-slot-names nil)
	      )

	  (with-*defstruct-accessors-iterated (accessor-function (pvar-structure-name pvar))
	    (push (eval `(alias!! (,accessor-function *pvar*)))
		  list-of-aliased-slot-pvars
		  ))

	  (with-*defstruct-slot-descriptors-iterated (slot-descriptor (pvar-structure-name pvar))
	    (push (*defstruct-slot-name slot-descriptor) list-of-slot-names)
	    )

	  (setq list-of-aliased-slot-pvars (nreverse list-of-aliased-slot-pvars))
	  (setq list-of-slot-names (nreverse list-of-slot-names))

	  (let ((number-of-pages (ceiling (- end start) per-line))
		(max-slot-name-length
		  (apply
		    'max
		    (mapcar
		      #'(lambda (x) (length (symbol-name x)))
		      list-of-slot-names
		      )))
		(index start)
		)

	    (fresh-line stream)

	    (cond
	      ((null title) nil)
	      ((eq t title) (format stream "~%*DEFSTRUCT ~S~%" (pvar-structure-name pvar)))
	      (t (format stream "~%~A~%" title))
	      )

	    (dotimes (j number-of-pages)
	      (format stream "~%")
	      (mapc
		#'(lambda (slot-name aliased-pvar)
		    (format stream "~S: " slot-name)
		    (dotimes (i (- max-slot-name-length (length (symbol-name slot-name))))
		      (format stream " ")
		      )
		    (dotimes (k per-line)
		      (when (< (+ index k) end)
			(format stream format-string (pref-or-junk aliased-pvar (+ index k)))
			))
		    (terpri stream)
		    )
		list-of-slot-names
		list-of-aliased-slot-pvars
		)
	      (incf index per-line)
	      )

	    ))))))


(defun print-pvar (pvar stream depth)
  depth
  (format stream "#<PVAR ~A ~S ~A ~A ~A>"
	  (pvar-name pvar)
	  (pvar-type pvar)
	  (case (pvar-type pvar)
	    (:general "")
	    (:array (format nil "~S" (pvar-array-dimensions pvar)))
	    (:structure (format nil "~S" (pvar-structure-name pvar)))
	    (otherwise "UNALLOCATED")
	    )
	  (if (pvar-vp-set pvar) (vp-set-name (pvar-vp-set pvar)) NIL)
	  (if (pvar-vp-set pvar) (vp-set-dimensions (pvar-vp-set pvar)) nil)
	  ))
