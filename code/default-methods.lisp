(in-package #:restricted-functions)

(defmethod original-function ((function function))
  function)

(defmethod mandatory-values ((function function))
  0)

(defmethod optional-values ((function function))
  0)

(defmethod rest-values-p ((function function))
  t)

(defmethod nth-value-type :before ((n integer) (function function))
  (check-type n (integer 0)))

(defmethod nth-argument-type :before ((n integer) (function function))
  (check-type n (integer 0)))

(defmethod nth-value-type ((n integer) (function function))
  (cond ((rest-values-p function)
         (if (< n call-arguments-limit)
             't
             'nil))
        ((< n (+ (mandatory-values function)
                 (optional-values function)))
         't)
        (t 'null)))

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
      &optional
      ,@(loop for n from mandatory-values
                below (+ mandatory-values optional-values)
              collect (nth-value-type n function))
      ,@(if rest-values-p '(&rest t) '()))))

(defmethod function-type ((function function))
  `(ftype ,(argument-types function)
          ,(values-type function)))
