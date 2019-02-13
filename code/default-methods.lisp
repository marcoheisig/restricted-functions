(in-package #:restricted-functions)

(defvar *default-strategy* (make-instance 'default-strategy))

(defmethod restrict :around (strategy function &rest argument-types)
  (if (functionp function)
      (if (null strategy)
          (apply #'restrict *default-strategy* function argument-types)
          (call-next-method))
      (if (null strategy)
          (apply #'restrict *default-strategy* (coerce function 'function) argument-types)
          (apply #'restrict strategy (coerce function 'function) argument-types))))

(defmethod infer-type :before (strategy (function function) &rest argument-types)
  (declare (ignore strategy))
  (check-arity function (length argument-types)))

(defmethod infer-type (strategy function &rest argument-types)
  (if (not (functionp function))
      (apply #'infer-type strategy (coerce function 'function) argument-types)
      (let ((inference-function (type-inference-function function)))
        (if (not inference-function)
            '(values &rest t)
            (apply inference-function argument-types)))))

(defmethod arity ((function function))
  (multiple-value-bind (mandatory maximal)
      (function-arity function)
    (if (= mandatory maximal)
        mandatory
        (error
         "~@<Only functions with a fixed number of arguments are permitted, ~
             but ~S takes between ~D and ~D arguments.~:@>"
         function mandatory maximal))))

(defmethod original-function ((function function))
  function)

(defmethod mandatory-values ((function function))
  0)

(defmethod optional-values ((function function))
  0)

(defmethod rest-values-p ((function function))
  t)

(defmethod nth-value-type ((n integer) (function function))
  (cond ((rest-values-p function)
         (if (< n call-arguments-limit)
             't
             'nil))
        ((< n (+ (mandatory-values function)
                 (optional-values function)))
         't)
        (t 'null)))

(defmethod nth-argument-type ((n integer) (function function))
  (assert (<= 0 n (1- (arity function))))
  't)

(defmethod argument-types ((function function))
  (loop for n below (arity function)
        collect (nth-argument-type n function)))

(defmethod values-type ((function function))
  (with-accessors ((mandatory-values mandatory-values)
                   (optional-values optional-values)
                   (rest-values-p rest-values-p))
      function
    `(values
      ,@(loop for n below mandatory-values
              collect (nth-value-type n function))
      ,@(if (zerop optional-values)
            '()
            `(&optional
              ,@(loop for n from mandatory-values
                        below (+ mandatory-values optional-values)
                      collect (nth-value-type n function))))
      ,@(if (not rest-values-p)
            '()
            '(&rest t)))))

(defmethod function-type ((function function))
  `(ftype ,(argument-types function)
          ,(values-type function)))
