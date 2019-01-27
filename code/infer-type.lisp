(in-package #:restricted-functions)

(defmethod infer-type :before ((function function) argument-types strategy)
  (check-arity function (length argument-types)))

(defmethod infer-type ((function function) argument-types strategy)
  '(values &rest t))
