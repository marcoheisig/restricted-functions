(cl:in-package #:common-lisp-user)

(cl:defpackage :restricted-functions
  (:use :closer-common-lisp)
  (:export

   ;; The restricted function protocol.
   #:restricted-function
   #:restricted-function-p
   #:restrict
   #:function-name
   #:original-function-name
   #:arity
   #:minimum-number-of-values
   #:maximum-number-of-values
   #:nth-value-type
   #:nth-argument-type
   #:argument-types
   #:value-types
   #:function-type

   ;; Type inference utilities.
   #:simplify-type-specifier
   #:simplified-type-of))
