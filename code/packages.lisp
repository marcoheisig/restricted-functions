(cl:in-package #:common-lisp-user)

(cl:defpackage #:restricted-functions
  (:use :closer-common-lisp)
  (:export
   ;; Classes
   #:restricted-function
   #:argument-type-simplification
   #:restricted-function-caching

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
