(asdf:defsystem #:univariate-decompositions
  :version "0"
  :description "Decompose the deviations of a R^n->R function from a trend."
  :maintainer "Tamas Papp <tkpapp@gmail.com>"
  :author "Tamas Papp <tkpapp@gmail.com>"
  :licence "BSD-style"
  :depends-on (#:alexandria
               #:array-operations
               #:clunit
               #:let-plus)
  :serial t
  :components ((:file "univariate-decompositions")))
