;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-i; MUSER: YES-*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;; The documentation for this stuff
;;; has been moved to segment-sets-doc.lisp

(*defstruct segment-set
  (start-bits nil :type boolean)
  (end-bits nil :type boolean)
  (processor-not-in-any-segment nil :type boolean)
  (start-address 0 :type (signed-byte 32) :cm-type (pvar (signed-byte (1+ *log-number-of-processors-limit*))))
  (end-address 0 :type (signed-byte 32) :cm-type (pvar (signed-byte (1+ *log-number-of-processors-limit*))))
  )


(*proclaim '(ftype (function (&rest t) (pvar segment-set)) create-segment-set!!))

(defun-wcefi create-segment-set!! (&key start-bit end-bit)

  (assert (or (pvarp start-bit) (pvarp end-bit)) () "You must provide either a start or end bit pvar")
  (if start-bit (assert (pvarp start-bit)))
  (if end-bit (assert (pvarp end-bit)))

  (let!! (segment-set-pvar)
    (declare (type (pvar segment-set) segment-set-pvar))

    (*all
      (*setf (segment-set-start-bits!! segment-set-pvar) nil!!)
      (*setf (segment-set-end-bits!! segment-set-pvar) nil!!)
      (*setf (segment-set-processor-not-in-any-segment!! segment-set-pvar) t!!)
      (*setf (segment-set-start-address!! segment-set-pvar) (!! -1))
      (*setf (segment-set-end-address!! segment-set-pvar) (!! -1))
      )

    ;; The user provided us only with start bits.

    (cond

      ((and start-bit (not end-bit))

       (*let (start)
	 (declare (type (pvar boolean) start))

	 (*nocompile (*set start start-bit))

	 (let (first-start-bit first-active-processor last-active-processor)
	   
	   (setq first-active-processor (*min (self-address!!)))
	   (setq last-active-processor (*max (self-address!!)))
	   
	   ;; if there are no active processors then no segments
	   ;; can be defined because no start-bits can be on.
	   
	   (when first-active-processor
	     
	     (*when start (setq first-start-bit (*min (self-address!!))))
	     
	     ;; if there is no start-bit on in the css then no segments
	     ;; can be defined.
	     
	     (when first-start-bit
	       
	       ;; put the start bits into the structure.
	       
	       (*setf (segment-set-start-bits!! segment-set-pvar) start)
	       
	       ;; figure out where the end bits must go.
	       ;; by definition the last active processor is the end
	       ;; of the segment defined by the last start-bit.
	       ;; Otherwise, except for the first start-bit, the
	       ;; processor before any processor containing a start-bit
	       ;; is the end of the previous segment.
	       
	       (*setf (pref (segment-set-end-bits!! segment-set-pvar) last-active-processor) t)
	       (*when (and!! start (/=!! (self-address!!) (!! (the fixnum first-start-bit))))
		 (*nocompile
		   (*pset :no-collisions t!! (alias!! (segment-set-end-bits!! segment-set-pvar)) (1-!! (self-address!!)))
		   ))
	       
	       ;; Turn on exactly those processors inside our newly defined segments.
	       
	       (*when (and!! (>=!! (self-address!!) (!! (the fixnum first-start-bit)))
			     (<=!! (self-address!!) (!! (the fixnum last-active-processor)))
			     )
		 
		 ;; copy-scan the start-of-segment's address to all processors
		 ;; in the segment.
		 
		 (*setf (segment-set-start-address!! segment-set-pvar)
			(scan!! (self-address!!) 'copy!!
				:segment-pvar
				(segment-set-start-bits!! (the (pvar segment-set) segment-set-pvar))
				))
		 
		 ;; copy-scan the end-of-segment's address to all processors
		 ;; in the segment.
		 
		 (*setf (segment-set-end-address!! segment-set-pvar)
			(scan!! (self-address!!) 'copy!!
				:direction :backward
				:segment-pvar
				(segment-set-end-bits!! segment-set-pvar)
				))
		 
		 ;; Identity all these processors as belonging to this
		 ;; segment set.
		 
		 (*setf (segment-set-processor-not-in-any-segment!! segment-set-pvar) nil!!) 
		 
		 ))))))

      ((and end-bit (not start-bit))

       (*let (end)
	 (declare (type (pvar boolean) end))

	 (*nocompile (*set end end-bit))

	 (let (last-end-bit first-active-processor last-active-processor)

	   (declare (ignore last-active-processor))
	   
	   (setq first-active-processor (*min (self-address!!)))
	   ;; (setq last-active-processor (*max (self-address!!)))
	   
	   ;; if there are no active processors then no segments
	   ;; can be defined because no start-bits can be on.
	   
	   (when first-active-processor
	     
	     (*when end (setq last-end-bit (*max (self-address!!))))
	     
	     ;; if there is no end-bit on in the css then no segments
	     ;; can be defined.
	     
	     (when last-end-bit
	       
	       ;; put the end bits into the structure.
	       
	       (*setf (segment-set-end-bits!! segment-set-pvar) end)
	       
	       ;; Figure out where the start bits must go.
	       ;; By definition the first active processor is the beginning
	       ;; of the segment defined by the first end-bit.
	       ;; Otherwise, except for the last end-bit, the
	       ;; processor after any processor containing an end-bit
	       ;; is the start of the next segment.
	       
	       (*setf (pref (segment-set-start-bits!! segment-set-pvar) first-active-processor) t)
	       (*when (and!! end (/=!! (self-address!!) (!! (the fixnum last-end-bit))))
		 (*nocompile
		   (*pset :no-collisions t!! (alias!! (segment-set-start-bits!! segment-set-pvar)) (1+!! (self-address!!)))
		   ))
	       
	       ;; Turn on exactly those processors inside our newly defined segments.
	       
	       (*when (and!! (<=!! (self-address!!) (!! (the fixnum last-end-bit)))
			     (>=!! (self-address!!) (!! (the fixnum first-active-processor)))
			     )
		 
		 ;; copy-scan the start-of-segment's address to all processors
		 ;; in the segment.
		 
		 (*setf (segment-set-start-address!! segment-set-pvar)
			(scan!! (self-address!!) 'copy!!
				:segment-pvar (the boolean-pvar (segment-set-start-bits!! segment-set-pvar))
				))
		 
		 ;; copy-scan the end-of-segment's address to all processors
		 ;; in the segment.
		 
		 (*setf (segment-set-end-address!! segment-set-pvar)
			(scan!! (self-address!!) 'copy!!
				:direction :backward
				:segment-pvar (the boolean-pvar (segment-set-end-bits!! segment-set-pvar))
				))
		 
		 ;; Identity all these processors as belonging to this
		 ;; segment set.
		 
		 (*setf (segment-set-processor-not-in-any-segment!! segment-set-pvar) nil!!) 

		 ))))))

      ((and start-bit end-bit)

       (*let (start end)
	 (declare (type (pvar boolean) start end))

	 (*all (*set start nil!!) (*set end nil!!))

	 (*nocompile
	   (*set start start-bit)
	   (*set end end-bit)
	   )

	 (let (first-active-processor #|last-active-processor|# first-start-bit last-start-bit first-end-bit last-end-bit)

	   (setq first-active-processor (*min (self-address!!)))
	   #|(setq last-active-processor (*max (self-address!!)))|#

	   (when first-active-processor

	     (*when start
	       (setq first-start-bit (*min (self-address!!)))
	       (setq last-start-bit (*max (self-address!!)))
	       )

	     (*when end
	       (setq first-end-bit (*min (self-address!!)))
	       (setq last-end-bit (*max (self-address!!)))
	       )

	     (when (or first-start-bit first-end-bit)

	       ;; by definition, if you specify both start and end bits, they
	       ;; must come in pairs.

	       (cond

		 ((null first-start-bit) (error "There are no start bits in the css, but there are end bits"))

		 ((null first-end-bit) (error "There are no end bits in the css, but there are start bits"))

		 ((> first-start-bit first-end-bit) (error "The first end bit is before the first start bit"))

		 ((> last-start-bit last-end-bit) (error "The last end bit is before the last start bit"))

		 (t

		  ;; make sure the start and stop bits are interleaved.

		  (*all

		    ;; Turn on every processor between the first start bit and the
		    ;; last end bit inclusive.

		    (*when (and!! (>=!! (self-address!!) (!! (the fixnum first-start-bit)))
				  (<=!! (self-address!!) (!! (the fixnum last-end-bit)))
				  )

		      (*let (sum-scan-result scan-source)
			(declare (type (pvar (signed-byte (1+ *current-send-address-length*))) sum-scan-result))
			(declare (type (pvar (signed-byte 8)) scan-source))

			;; Make a pvar with 1 in every start processor which
			;; is not also an end processor, -1 in every end processor
			;; which is not also a start processor, and 0 elsewhere.

			(*set scan-source (!! 0))
			(*when (and!! start (not!! end)) (*set scan-source (!! 1)))
			(*when (and!! end (not!! start)) (*set scan-source (!! -1)))

			;; Now scan across all the active processors.
			;; The idea is that if there is an end processor after
			;; every start processor then the end processors
			;; will sum out to 0, and the start processors
			;; will be unchanged.
			
			(*set sum-scan-result (scan!! scan-source '+!!))
			(*when (and!! start (not!! end))
			  (when (*or (/=!! (!! 1) sum-scan-result))
			    (error "There is no end-bit between two start bits.  They must be interleaved.")
			    ))
			(*when (and!! end (not!! start))
			  (when (*or (/=!! (!! 0) sum-scan-result))
			    (error "There is not start bit between two end bits.  They must be interleaved.")
			    ))

			)

		      ;; Ok, each start processor has an end processor
		      ;; either coincident with it or after it and
		      ;; before the next start processor.  Therefore
		      ;; the start and end bits are valid and we can
		      ;; store them away into our structure.

		      (*setf (segment-set-start-bits!! segment-set-pvar) start)
		      (*setf (segment-set-end-bits!! segment-set-pvar) end)

		      ;; We want to determine regions which are not within
		      ;; these (start - end) segments.

		      ;; The active set is currently all processors between the first
		      ;; start bit and the last end bit.

		      (*let ((inside-defined-segment? t!!))
			(declare (type (pvar boolean) inside-defined-segment?))
			
			(*when end (*set inside-defined-segment? nil!!))

			;; this will cause every processor after and including each end processor
			;; and before the next start processor to have a nil in
			;; inside-defined-segment?.

			(*set inside-defined-segment?
			      (scan!! inside-defined-segment? 'and!! :segment-pvar start)
			      )

			(*when end (*set inside-defined-segment? t!!))

 			(*setf (segment-set-processor-not-in-any-segment!! segment-set-pvar) (not!! inside-defined-segment?))

			)

		      (*when (not!! (the boolean-pvar (segment-set-processor-not-in-any-segment!! segment-set-pvar)))

			;; copy-scan the start-of-segment's address to all processors
			;; in the segment.
		 
			(*setf (segment-set-start-address!! segment-set-pvar)
			       (scan!! (self-address!!) 'copy!!
				       :segment-pvar (the boolean-pvar (segment-set-start-bits!! segment-set-pvar))
				       ))
		 
			;; copy-scan the end-of-segment's address to all processors
			;; in the segment.
			
			(*setf (segment-set-end-address!! segment-set-pvar)
			       (scan!! (self-address!!) 'copy!!
				       :direction :backward
				       :segment-pvar (the boolean-pvar (segment-set-end-bits!! segment-set-pvar))
				       ))

			))))))))

	 ))

      )

    segment-set-pvar

    ))



(defun-wcefi segment-set-scan!!

       ;; What does this do?  We want to be able to execute a scan
       ;; inside a set of segments (which are each a set of contiguous processors)
       ;; previously defined using the create-segment-set!! function,
       ;; and do it independently of the existing css.  The scan
       ;; is performed over all the processors in each segment.

       ;; An option is provided which allows the user to do the
       ;; scan operation only in those processors both active
       ;; when the function is entered and inside one of the
       ;; segments defined by the segent set.  This option is
       ;; enabled using the :activate-all-processors-in-segment-set
       ;; keyword by giving it a value of NIL.

       ;; Unless told to do so, the routine will not check for active
       ;; processors outside the segment set.  These processors will
       ;; not participate in any way in the scan operation.  If told
       ;; to check and processors are found outside the segment set's
       ;; scope, the routine will error out.

       (pvar
	scan-operator
	segment-set-pvar
	&key
	(direction :forward)
	(check-for-processors-not-in-segment-set nil)
	(activate-all-processors-in-segment-set t)
	)

  (*locally
    (declare (type (pvar segment-set) segment-set-pvar))

    ;; Save away the CSS.

    (*let (css-upon-entering-rational-scan!!)
      (declare (type (pvar boolean) css-upon-entering-rational-scan!!))
      (declare (return-pvar-p t))

      (*all (*set css-upon-entering-rational-scan!! nil!!))
      (*set css-upon-entering-rational-scan!! t!!)

      (all!!

	(when check-for-processors-not-in-segment-set
	  (when (*or (and!! (segment-set-processor-not-in-any-segment!! segment-set-pvar)
			    css-upon-entering-rational-scan!!
			    ))
	    (error "There is an active processor not within the segment set defined by the segment-set-pvar")
	    ))

	;; Select exactly those processors defined to belong
	;; to the segments in the segment set.

	(when!! (not!! (segment-set-processor-not-in-any-segment!! segment-set-pvar))

	  (if activate-all-processors-in-segment-set

	      ;; Do the scan over each segment.

	      (scan!!
		pvar
		scan-operator
		:direction direction
		:segment-pvar (if (eq direction :forward)
				  (segment-set-start-bits!! segment-set-pvar)
				  (segment-set-end-bits!! segment-set-pvar)
				  )
		:include-self t
		)

	      ;; We need to activate only those processors both in the old CSS
	      ;; and belonging to the segments in the segment set.

	      ;; So we initialize a pvar to be 0 except in processors which
	      ;; were active under the old CSS and are in the segment set;
	      ;; these we set to 1.

	      (let!! ((sum-scan-result (!! 0)))
		(declare (type (pvar (unsigned-byte *log-number-of-processors-limit*)) sum-scan-result))
	      
		(*when css-upon-entering-rational-scan!!
		  (declare (return-pvar-p nil))
		  (*set sum-scan-result (!! 1))
		  )
	      
		;; We need to find the first (or last, if we are backward scanning)
		;; processor active under the old CSS in each segment.  So we do
		;; a sum scan, and every processor but the first (last) will have
		;; something greater than 1.  Clever, eh?
	      
		(if (eq direction :forward)

		    (*set sum-scan-result
			  (scan!!
			    sum-scan-result
			    '+!!
			    :direction :forward
			    :segment-pvar (segment-set-start-bits!! segment-set-pvar)
			    ))

		    (*set sum-scan-result
			  (scan!!
			    sum-scan-result
			    '+!!
			    :direction :backwards
			    :segment-pvar (segment-set-end-bits!! segment-set-pvar)
			    ))

		    )

		;; the processor in each segment which has value 1 is the first (or last) active
		;; processor in that segment and within css-upon-entering-rational-scan!!.
		;; Use a pvar which is T for these processors and NIL otherwise as
		;; the segment pvar to the regular scan function.
	      
		(when!! css-upon-entering-rational-scan!!
		
		  (let!! ((segment-pvar (=!! sum-scan-result (!! 1))))
		    (declare (type (pvar boolean) segment-pvar))
		  
		    (scan!!
		      pvar
		      scan-operator
		      :direction direction
		      :segment-pvar segment-pvar
		      )
		  
		    )))))))))



;(defun test-segment-sets ()
;
;  (*let ((segment-set
;	   (create-segment-set!!
;	     :start-bit (zerop!! (mod!! (self-address!!) (!! 7)))
;	     :end-bit (=!! (!! 3) (mod!! (self-address!!) (!! 7)))
;	     )))
;
;    (ppp (segment-set-start-bits!! segment-set) :end 30)
;    (ppp (segment-set-end-bits!! segment-set) :end 30)
;    (ppp (segment-set-processor-not-in-any-segment!! segment-set) :end 30)
;    (ppp (segment-set-start-address!! segment-set) :end 30)
;    (ppp (segment-set-end-address!! segment-set) :end 30)
;
;    (*let ((temp (!! 0)))
;      (declare (type (pvar (unsigned-byte 32)) temp))
;      (*set temp (segment-set-scan!! (self-address!!) 'copy!! segment-set :direction :forward))
;      (ppp temp :end 50)
;      (*set temp (!! 0))
;      (*when (oddp!! (self-address!!))
;	(*set temp
;	      (segment-set-scan!!
;		(self-address!!) 'copy!! segment-set :direction :backward :activate-all-processors-in-segment-set nil
;		)))
;      (ppp temp :end 50)
;      ))
;
;  (*when (not!! (zerop!! (self-address!!)))
;    (let ((segment-set
;	     (create-segment-set!! :end-bit (=!! (!! 4) (mod!! (self-address!!) (!! 6))))
;	     ))
;
;    (ppp (alias!! (segment-set-start-bits!! segment-set)) :end 30)
;    (ppp (alias!! (segment-set-end-bits!! segment-set)) :end 30)
;    (ppp (alias!! (segment-set-processor-not-in-any-segment!! segment-set)) :end 30)
;    (ppp (alias!! (segment-set-start-address!! segment-set)) :end 30)
;    (ppp (alias!! (segment-set-end-address!! segment-set)) :end 30)
;
;    (ppp (segment-set-scan!! (self-address!!) 'copy!! segment-set :direction :backward) :end 40)
;
;    )))



