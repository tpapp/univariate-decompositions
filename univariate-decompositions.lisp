;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:univariate-decompositions
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:deviations))

(cl:in-package #:univariate-decompositions)

(defun deviations (f x g &optional (element-type t))
  ""
  (declare (optimize debug))
  (check-type x (array * (* *)))
  (let+ ((vectors (ao:split x 1))
         (trends (ao:each g vectors))
         ((&flet v- (vector1 vector2)
            (ao:each* element-type #'- vector1 vector2)))
         ((&flet f-on-vectors (vectors)
            (ao:margin* element-type f (ao:combine vectors element-type) 0)))
         (f-trend (f-on-vectors trends))
         ((&flet f-deviation (vectors)
            (v- (f-on-vectors vectors) f-trend)))
         (lhs (f-deviation vectors))
         (rhs (ao:combine (ao:generate (lambda (vector-index)
                                         (let ((v (copy-array trends)))
                                           (setf (aref v vector-index)
                                                 (aref vectors vector-index))
                                           (f-deviation v)))
                                       vectors
                                       :position)
                          element-type))
         (rhs-sums (ao:margin* element-type (curry #'reduce #'+) rhs 0))
         (rhs-error (v- lhs rhs-sums)))
    (values lhs rhs rhs-error)))

(cl:defpackage #:univariate-decompositions-tests
  (:use #:cl
        #:alexandria
        #:anaphora
        #:clunit
        #:let-plus)
  (:export
   #:run))

(cl:in-package #:univariate-decompositions-tests)

(defsuite tests ())

(defun run (&optional interactive?)
  (run-suite 'tests :use-debugger interactive?))

(deftest test1 (tests)
  (let+ (((&flet g (vector)
            (ao:recycle (/ (reduce #'+ vector) (length vector))
                        :outer vector)))
         (a #(0 1 2))                   ; level: 1
         (b #(3 5 7))                   ; level: 5
         ((&flet f (v)
            (let+ ((#(v0 v1) v))
              (+ (* 3 v0) (expt v1 2)))))
         (x (ao:combine (vector a b)))
         ((&values lhs rhs rhs-error)
          (univariate-decompositions:deviations #'f x #'g)))
    ;; Calculations:
    ;; f(x) is #(9 28 55)
    ;; f(g(x)) (the trend) is #(28 28 28)
    ;; lhs is #(-19 0 27)
    ;; rhs+f(g(x)) is #(#(25 28 31) #(12 28 52))
    ;; rhs is #2A((-3 0 3) (16 0 24))
    ;; error is always 0 (f is additively separable)
    (assert-equalp #(-19 0 27) lhs)
    (assert-equalp #2A((-3 0 3) (-16 0 24)) rhs)
    (assert-equalp #(0 0 0) rhs-error)))
