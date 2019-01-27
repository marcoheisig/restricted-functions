(in-package #:restricted-functions)

(defclass restricted-function (funcallable-standard-object)
  ((%name :initform nil :reader function-name))
  (:metaclass funcallable-standard-class))

(defmethod shared-initialize :after
    ((instance restricted-function) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (ensure-restricted-function-name instance)
  (ensure-restricted-function-instance-function instance)
  (ensure-restricted-function-fdefinition instance)
  (ensure-restricted-function-compiler-macro instance))

(defmethod print-object ((rf restricted-function) stream)
  (print-unreadable-object (rf stream :type t :identity t)
    (write (function-name rf) :stream stream)))

(defun ensure-restricted-function-name (rf)
  (unless (slot-boundp rf '%name))
  (setf (slot-value rf '%name)
        (compute-restricted-function-name
         (original-function-name rf)
         (argument-types rf))))

(defun compute-restricted-function-name (fname types)
  (values
   (intern
    (with-output-to-string (stream)
      (with-standard-io-syntax
        (format stream "~A::~A ~{~A~^ ~}"
                (package-name (symbol-package fname))
                (symbol-name fname)
                types)))
    :restricted-functions)))

(defun ensure-restricted-function-instance-function (rf)
  (set-funcallable-instance-function
   rf
   (let ((fn (original-function-name rf)))
     (lambda (&rest args)
       (apply fn args)))))

(defun ensure-restricted-function-fdefinition (rf)
  (setf (fdefinition (function-name rf)) rf))

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

