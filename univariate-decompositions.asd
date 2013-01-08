(asdf:defsystem #:univariate-decompositions
  :version "0"
  :description ""
  :maintainer "Tamas Papp <tkpapp@gmail.com>"
  :author "Tamas Papp <tkpapp@gmail.com>"
  :licence "BSD-style"
  :depends-on (#:alexandria
               #:anaphora
               #:array-operations
               #:clunit
               #:let-plus)
  :serial t
  :components ((:file "univariate-decompositions")))
