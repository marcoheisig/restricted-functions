(cl:in-package #:common-lisp-user)

(cl:defpackage #:restricted-functions
  (:use :closer-common-lisp)
  (:export
   #:restricted-function
   #:restrict
   #:function-name
   #:original-function-name
   #:arity
   #:mandatory-values
   #:optional-values
   #:rest-values-p
   #:nth-value-type
   #:nth-argument-type
   #:argument-types
   #:value-types
   #:function-type))
