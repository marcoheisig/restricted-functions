(in-package #:restricted-functions)

(defclass restricted-function (funcallable-standard-object)
  ((%name :initarg :name :reader name)
   (%original-function :initarg :original-function :reader original-function)
   (%argument-types :initarg :argument-types :reader argument-types)
   (%result-types :initarg :result-types :reader result-types)
   (%mandatory-values :initarg :mandatory-values :reader mandatory-values)
   (%optional-values :initarg :optional-values :reader optional-values)
   (%rest-values-p :initarg :rest-values-p :reader rest-values-p))
  (:metaclass funcallable-standard-class))

(defmethod print-object ((rf restricted-function) stream)
  (print-unreadable-object (rf stream :type t :identity t)
    (format stream "~S (~{~S~^ ~})"
            (name rf)
            (coerce (argument-types rf) 'list))))

(defun make-restricted-function
    (original-function argument-types values-type)
  (multiple-value-bind (mandatory optional rest-values-p)
      (parse-values-type values-type)
    (let* ((name (generate-restricted-function-name))
           (rf (make-instance 'restricted-function
                 :name name
                 :original-function (coerce original-function 'function)
                 :argument-types (coerce argument-types 'simple-vector)
                 :result-types (concatenate 'simple-vector mandatory optional)
                 :mandatory-values (length mandatory)
                 :optional-values (length optional)
                 :rest-values-p rest-values-p)))
      (set-funcallable-instance-function rf original-function)
      (setf (fdefinition name) rf)
      (setf (compiler-macro-function name)
            (compute-compiler-macro-function rf))
      rf)))

(defun parse-values-type (values-type)
  (if (and (consp values-type)
           (eq (car values-type) 'values))
      (labels ((mandatory (rest mandatory)
                 (if (null rest)
                     (done mandatory '() nil)
                     (case (car rest)
                       (&optional (optional rest mandatory '()))
                       (&rest (done mandatory '() t))
                       (otherwise (mandatory (cdr rest) (cons (car rest) mandatory))))))
               (optional (rest mandatory optional)
                 (if (null rest)
                     (done mandatory optional nil)
                     (case (car rest)
                       (&rest (done mandatory optional t))
                       (otherwise (optional (cdr rest) mandatory (cons (car rest) optional))))))
               (done (mandatory optional rest-p)
                 (values (nreverse mandatory)
                         (nreverse optional)
                         rest-p)))
        (mandatory (rest values-type) '()))
      (values values-type nil nil)))

(defun generate-restricted-function-name ()
  (loop
    (multiple-value-bind (symbol status)
        (intern (genstring "RESTRICTED-FUNCTION-") #.*package*)
      (when (null status)
        (return symbol)))))

(defun genstring (&optional (prefix "G"))
  (with-output-to-string (stream)
    (format stream "~A~X" prefix (random most-positive-fixnum))))

(defun compute-compiler-macro-function (rf)
  (let ((values-type-specifier (values-type rf))
        (argument-types (argument-types rf))
        (original-function (original-function rf))
        (arity (arity rf)))
    (lambda (form environment)
      (declare (ignore environment))
      (destructuring-bind (function &rest arguments) form
        (declare (ignore function))
        (assert (= (length arguments) arity))
        `(the ,values-type-specifier
              (funcall ,original-function
                       ,@(loop for argument in arguments
                               for argument-type in argument-types
                               collect
                               `(the ,argument-type ,argument))))))))

(defmethod arity ((rf restricted-function))
  (length
   (argument-types rf)))
