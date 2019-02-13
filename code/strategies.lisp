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
  (let ((key (list* function argument-types))
        (cache (restricted-function-cache strategy)))
    (multiple-value-bind (value present-p)
        (gethash key cache)
      (if present-p
          value
          (setf (gethash key cache) (call-next-method))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default Behavior

(defclass default-strategy
    (argument-type-simplification
     restricted-function-caching)
  ())

(defvar *default-strategy* (make-instance 'default-strategy))
