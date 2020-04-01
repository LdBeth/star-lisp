(in-package :asdf)

(defsystem f20-examples
  :depends-on ("f20")
  :components
  ((:module "examples"
    :components
    ((:file "text-processing-example")
     (:file "bitblt-example")
     ;; (:file "ca-example")
     (:file "canon-mm-example")
     (:file "combinations-example")
     (:file "primes-example")
     (:file "timing-example")
     (:file "very-long-addition-example")))))
