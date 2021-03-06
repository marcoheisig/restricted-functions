(in-package #:restricted-functions)

(defclass restricted-function (funcallable-standard-object)
  ((%name :initarg :name :reader name)
   (%original-function :initarg :original-function :reader original-function)
   (%mandatory-values :initarg :mandatory-values :reader mandatory-values)
   (%optional-values :initarg :optional-values :reader optional-values)
   (%rest-values-p :initarg :rest-values-p :reader rest-values-p)
   (%atypes :initarg :atypes :reader atypes :type simple-vector)
   (%rtypes :initarg :rtypes :reader rtypes :type simple-vector))
  (:metaclass funcallable-standard-class))

(defmethod print-object ((rf restricted-function) stream)
  (print-unreadable-object (rf stream :type t :identity t)
    (format stream "~S ~{~S~^ ~}"
            (or (nth-value 2 (function-lambda-expression (original-function rf)))
                (original-function rf))
            (coerce (argument-types rf) 'list))))

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
      (values (list values-type) nil nil)))

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
  (length (atypes rf)))

(defmethod nth-value-type ((n integer) (rf restricted-function))
  (with-accessors ((rtypes rtypes)) rf
    (if (< n (length rtypes))
        (svref rtypes n)
        (if (rest-values-p rf)
            't
            'null))))

(defmethod nth-argument-type ((n integer) (rf restricted-function))
  (svref (atypes rf) n))
