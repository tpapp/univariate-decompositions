(asdf:defsystem #:univariate-contributions
  :version "0"
  :description ""
  :maintainer "Tamas Papp <tkpapp@gmail.com>"
  :author "Tamas Papp <tkpapp@gmail.com>"
  :licence "BSD-style"
  :depends-on (#:alexandria
               #:anaphora
               #:array-operations
               #:let-plus)
  :serial t
  :components ((:file "univariate-contributions")))

;; (asdf:defsystem #:univariate-contributions-tests
;;   :version "0"
;;   :description "Unit tests for UNIVARIATE-CONTRIBUTIONS."
;;   :maintainer "Tamas Papp <tkpapp@gmail.com>"
;;   :author "Tamas Papp <tkpapp@gmail.com>"
;;   :licence "BSD-style"
;;   :depends-on (#:clunit
;;                #:univariate-contributions)
;;   :serial t
;;   :components ((:file "univariate-contributions-tests")))
