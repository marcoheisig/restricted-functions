(cl:in-package #:common-lisp-user)

(cl:defpackage #:restricted-functions
  (:use :closer-common-lisp)
  (:export
   #:restricted-function
   ;; Strategies
   #:argument-type-simplification-around-restrict
   #:argument-type-simplification-around-infer-type
   #:restricted-function-caching
   #:default-type-inference

   ;; Generic Functions
   #:restrict
   #:infer-type
   #:name
   #:original-function
   #:arity
   #:mandatory-values
   #:optional-values
   #:rest-values-p
   #:nth-value-type
   #:nth-argument-type
   #:argument-types
   #:values-type
   #:function-type))
