;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:univariate-decompositions
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:deviations))

(cl:in-package #:univariate-decompositions)

(defun deviations (function vectors trend &optional (element-type t))
  "Decompose deviations from the trend.  The exact procedure is described in the README.  Function is f (mapping a vector to a scalar), the x's are in VECTORS (a sequence of vectors), g calculates the trend (mapping a vector to a vector).  When ELEMENT-TYPE is given, the results of all interim calculations have to be subtypes of that (or arrays of that subtype).

Return (values v w e)."
  (let+ ((vectors (aprog1 (coerce vectors 'vector)
                    (assert (plusp (length it)))
                    (assert (every #'vectorp it))
                    (assert (every (curry #'length= (length (aref it 0)))
                                   (subseq it 1)))))
         (trends (ao:each trend vectors))
         ((&flet v- (vector1 vector2)
            (ao:each* element-type #'- vector1 vector2)))
         ((&flet f-on-vectors (vectors)
            (ao:margin* element-type function (ao:combine vectors element-type) 0)))
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
         (x (vector a b))
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

(deftest test2 (tests)
  (let+ (((&flet g (vector)
            (ao:recycle 2 :outer vector)))
         ((&flet f (v)
            (* (aref v 0) (aref v 1))))
         ((&values lhs rhs rhs-error)
          (univariate-decompositions:deviations #'f #(#(3) #(5)) #'g)))
    ;; extremely simple calculation, but with interactions (and thus error)
    (assert-equalp #(11) lhs)
    (assert-equalp #2A((2) (6)) rhs)
    (assert-equalp #(3) rhs-error)))
