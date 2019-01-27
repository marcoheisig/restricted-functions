(in-package #:restricted-functions)

(defgeneric restricted-function-cache-value (cache function argument-types strategy))

(defgeneric (setf restricted-function-cache-value) (value cache function argument-types strategy))

(defclass restricted-function-cache ()
  ())
