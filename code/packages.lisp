(cl:in-package #:common-lisp-user)

(cl:defpackage :restricted-functions
  (:use :closer-common-lisp)
  (:export
   #:restrict
   #:restricted-function
   #:restricted-function-p
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
   #:defrestrictor
   #:ensure-restrictor))
