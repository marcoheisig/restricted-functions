(in-package #:restricted-functions)

(declaim (hash-table *restrictor-table*))

(defvar *restrictor-table* (make-hash-table :test #'eq))

(defmacro defrestrictor (function-name (function-name-var type-var) &body body)
  (check-type function-name-var symbol)
  (check-type type-var symbol)
  `(ensure-restrictor
    ',function-name
    (lambda (,function-name-var ,type-var)
      ,@body)))

(defun ensure-restrictor (original-function-name function)
  (check-type original-function-name symbol)
  (check-type function function)
  (setf (gethash (fdefinition original-function-name) *restrictor-table*)
        (lambda (types)
          (funcall function original-function-name types))))

(defun find-restrictor (function-designator)
  (values
   (gethash (coerce function-designator 'function) *restrictor-table*)))

(defmethod restrict ((rf restricted-function) (argument-types list))
  (restrict (original-function-name rf) argument-types))

(defmethod restrict ((function function) (argument-types list))
  (let ((restrictor (find-restrictor function)))
    (if (null restrictor)
        function
        (funcall restrictor argument-types))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Caching

(declaim (hash-table *restricted-function-cache*))

(defvar *restricted-function-cache* (make-hash-table :test #'equal))

(defmethod restrict :around ((object t) (argument-types list))
  (let ((key (cons object argument-types)))
    (multiple-value-bind (value present-p)
        (gethash key *restricted-function-cache*)
      (if (not present-p)
          (let ((value (call-next-method)))
            (setf (gethash key *restricted-function-cache*) value))
          value))))
