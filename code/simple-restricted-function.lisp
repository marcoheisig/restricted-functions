(in-package #:restricted-functions)

(defclass simple-restricted-function (restricted-function)
  ((%original-name :initarg :original-name :reader original-function-name
                   :initform (alexandria:required-argument 'original-function-name))
   (%argument-types :initarg :argument-types :reader argument-types
                    :initform (alexandria:required-argument 'argument-types))
   (%value-types :initarg :value-types :reader value-types
                 :initform (alexandria:required-argument 'value-types)))
  (:metaclass funcallable-standard-class))

(defmethod arity ((srf simple-restricted-function))
  (length (argument-types srf)))

(defmethod minimum-number-of-values ((srf simple-restricted-function))
  (length
   (value-types srf)))

(defmethod maximum-number-of-values ((srf simple-restricted-function))
  (length
   (value-types srf)))

(defmethod nth-value-type ((n integer) (srf simple-restricted-function))
  (nth n (value-types srf)))

(defmethod nth-argument-type ((n integer) (srf simple-restricted-function))
  (nth n (argument-types srf)))

(defun make-simple-restricted-function
    (original-name argument-types value-types)
  (make-instance 'simple-restricted-function
    :original-name original-name
    :argument-types argument-types
    :value-types value-types))

(defmacro define-type-inference-rule
    ((function-name argument-types &optional (strategy-specializer 'null))
     &body body)
  (check-type function-name symbol)
  (check-type argument-types symbol)
  (check-type strategy-specializer (or symbol class))
  (assert (fboundp function-name))
  (alexandria:with-gensyms (function strategy argtypes valtypes)
    `(defmethod restrict-using-strategy
         ((,function (eql #',function-name))
          (,argument-types list)
          (,strategy ,strategy-specializer))
       (declare (ignore ,function ,strategy))
       (let ((,argtypes ,argument-types)
             (,valtypes (the list (progn ,@body))))
         (make-simple-restricted-function ',function-name ,argtypes ,valtypes)))))
