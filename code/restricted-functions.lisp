;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :restricted-functions)

(defclass restricted-function (funcallable-standard-object)
  ((%name :reader function-name))
  (:metaclass funcallable-standard-class))

(defmethod restricted-function-p ((rf restricted-function))
  t)

(defmethod slot-unbound ((class funcallable-standard-class)
                         (instance restricted-function)
                         (slot-name (eql '%name)))
  (setf (slot-value instance '%name)
        (compute-restricted-function-name
         (original-function-name instance)
         (argument-types instance))))

(defun compute-restricted-function-name (base-name types)
  (values
   (intern
    (with-output-to-string (stream)
      (with-standard-io-syntax
        (format stream "~A::~A ~{~A~^ ~}"
                (package-name (symbol-package base-name))
                (symbol-name base-name)
                types)))
    :restricted-functions)))

(defmethod shared-initialize :after
    ((instance restricted-function) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (ensure-restricted-function-instance-function instance)
  (ensure-restricted-function-fdefinition instance)
  (ensure-restricted-function-proclamation instance)
  (ensure-restricted-function-compiler-macro instance))

(defun ensure-restricted-function-instance-function (rf)
  (set-funcallable-instance-function
   rf
   (let ((fn (original-function-name rf)))
     (lambda (&rest args)
       (apply fn args)))))

(defun ensure-restricted-function-fdefinition (rf)
  (setf (fdefinition (function-name rf)) rf))

(defun ensure-restricted-function-proclamation (rf)
  (proclaim `(type ,(function-type rf) ,(function-name rf))))

(defun ensure-restricted-function-compiler-macro (rf)
  (setf (compiler-macro-function (function-name rf))
        (let ((values-type-specifier `(values ,@(values-types rf)))
              (argument-types (argument-types rf))
              (original-function (original-function-name rf))
              (arity (arity rf)))
          (lambda (form environment)
            (declare (ignore environment))
            (destructuring-bind (function &rest arguments) form
              (declare (ignore function))
              (assert (= (length arguments) arity))
              `(the
                ,values-type-specifier
                (,original-function
                 ,@(loop for argument in arguments
                         for argument-type in argument-types
                         collect `(the ,argument-type ,argument)))))))))

(defclass simple-restricted-function (restricted-function)
  ((%original-name :initarg :original-name :reader original-function-name
                   :initform (alexandria:required-argument 'original-function-name))
   (%argument-types :initarg :argument-types :reader argument-types
                    :initform (alexandria:required-argument 'argument-types))
   (%min-values :initarg :min-values :reader minimum-number-of-values
                :initform (alexandria:required-argument 'minimum-number-of-values))
   (%max-values :initarg :max-values :reader maximum-number-of-values
                :initform (alexandria:required-argument 'maximum-number-of-values))
   (%value-types :initarg :value-types :reader value-types
                 :initform (alexandria:required-argument 'value-types)))
  (:metaclass funcallable-standard-class))

(defmethod arity ((srf simple-restricted-function))
  (length (argument-types srf)))
