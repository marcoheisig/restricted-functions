(in-package #:restricted-functions)

(defparameter *type-inference-functions*
  (make-hash-table :test #'eq))

(defmacro type-inference-function (function)
  `(values (gethash ,function *type-inference-functions*)))

(defmacro define-type-inference-rule (fname args &body body)
  `(setf (type-inference-function #',fname)
         (lambda ,args ,@body)))
