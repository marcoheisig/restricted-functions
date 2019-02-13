(in-package #:restricted-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simplification of Argument Types

(defclass argument-type-simplification ()
  ())

(defmethod restrict :around
    ((strategy argument-type-simplification)
     (function function)
     &rest argument-types)
  (apply #'call-next-method
         strategy
         function
         (mapcar #'simplified-types:simplify-type argument-types)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Caching of Restricted Functions

(defclass restricted-function-caching ()
  ((%cache :initform (make-hash-table :test #'equal)
           :reader restricted-function-cache)))

(defmethod restrict :around
    ((strategy restricted-function-caching)
     (function function)
     &rest argument-types)
  ;; TODO
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default Behavior

(defclass default-strategy
    (argument-type-simplification
     restricted-function-caching)
  ())
