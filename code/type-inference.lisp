(in-package #:restricted-functions)

;;; This file defines the default behavior for the generic function
;;; RESTRICT-USING-STRATEGY when no strategy is specified.

(define-type-inference-rule (+ argument-types)
  '(t))
