(in-package #:restricted-functions)

(defmethod restrict-using-strategy :before
    ((function function) (argument-types list) strategy)
  (declare (ignore strategy))
  (check-arity function (length argument-types)))

(defmethod minimum-number-of-values ((function function))
  0)

(defmethod maximum-number-of-values ((function function))
  (1- multiple-values-limit))

(defmethod nth-value-type :before ((n integer) (function function))
  (assert (<= 0 n)))

(defmethod nth-argument-type :before ((n integer) (function function))
  (assert (<= 0 n)))

(defmethod nth-value-type ((n integer) (function function))
  (cond ((< n (maximum-number-of-values function)) 't)
        (t 'null)))

(defmethod argument-types ((function function))
  (loop for n below (arity function)
        collect (nth-argument-type n function)))

(defmethod values-types ((function function))
  `(,@(loop for n below (minimum-number-of-values function)
            collect (nth-value-type n function))
    ,@(if (= (minimum-number-of-values function)
             (maximum-number-of-values function))
          '()
          '(&rest t))))

(defmethod function-type ((function function))
  `(ftype ,(argument-types function)
          ,@(values-types function)))
