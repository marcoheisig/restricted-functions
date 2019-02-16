(in-package #:restricted-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simplification of Argument Types

(defclass argument-type-simplification-around-restrict ()
  ())

(defmethod restrict :around
    ((strategy argument-type-simplification-around-restrict)
     (function function)
     &rest argument-types)
  (apply #'call-next-method
         strategy
         function
         (mapcar #'simplified-types:simplify-type argument-types)))

(defclass argument-type-simplification-around-infer-type ()
  ())

(defmethod infer-type :around
    ((strategy argument-type-simplification-around-infer-type)
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
          (setf (gethash key cache)
                (call-next-method))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type Inference using Simplified Types

(defclass default-type-inference ()
  ())

(defparameter *type-inference-functions*
  (make-hash-table :test #'eq))

(defmacro type-inference-function (function)
  `(values (gethash ,function *type-inference-functions*)))

(defmacro define-type-inference-rule (fname args &body body)
  `(setf (type-inference-function #',fname)
         (lambda ,args ,@body)))

(defmethod infer-type ((strategy default-type-inference) (function function)
                       &rest argument-types)
  (check-arity function (length argument-types))
  (let ((inference-function (type-inference-function function)))
    (if (not inference-function)
        (call-next-method)
        (apply inference-function argument-types))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default Behavior

(defclass default-strategy-for-restrict
    (argument-type-simplification-around-restrict
     restricted-function-caching
     default-type-inference)
  ())

(defclass default-strategy-for-infer-type
    (argument-type-simplification-around-infer-type
     default-type-inference)
  ())

(defvar *default-strategy-for-restrict*
  (make-instance 'default-strategy-for-restrict))

(defvar *default-strategy-for-infer-type*
  (make-instance 'default-strategy-for-infer-type))
