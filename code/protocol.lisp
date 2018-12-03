;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :restricted-functions)

(defgeneric restrict (function argument-types)
  (:method ((symbol symbol) (argument-types list))
    (restrict (fdefinition symbol) argument-types))
  (:documentation
   "Return a function with the same semantics as FUNCTION, but whose domain
is restricted to arguments of the given ARGUMENT-TYPES.  If possible, the
result is a subclass of RESTRICTED-FUNCTION and can be queried for a
variety of information."))

(defgeneric restricted-function-p (object)
  (:documentation
   "Return whether OBJECT is a restricted function."))

(defgeneric function-name (function)
  (:method ((symbol symbol))
    (function-name (fdefinition symbol)))
  (:documentation
   "Return a symbol that is the name of FUNCTION."))

(defgeneric original-function-name (function)
  (:method ((symbol symbol))
    (original-function-name (fdefinition symbol)))
  (:documentation
   "Return the name of the function upon which FUNCTION specializes."))

(defgeneric arity (function)
  (:method ((symbol symbol))
    (arity (fdefinition symbol)))
  (:documentation
   "Return the number of arguments of FUNCTION."))

(defgeneric minimum-number-of-values (function)
  (:method ((symbol symbol))
    (minimum-number-of-values (fdefinition symbol)))
  (:documentation
   "Return an inclusive lower bound for the number of values returned by FUNCTION."))

(defgeneric maximum-number-of-values (function)
  (:method ((symbol symbol))
    (maximum-number-of-values (fdefinition symbol)))
  (:documentation
   "Return an inclusive upper bound for the number of values returned by FUNCTION."))

(defgeneric nth-value-type (n function)
  (:method ((n integer) (symbol symbol))
    (nth-value-type n (fdefinition symbol)))
  (:documentation
   "Return a type specifier that describes the Nth value returned by the
function described by RESTRICTED-FUNCTION."))

(defgeneric nth-argument-type (n function)
  (:method ((n integer) (symbol symbol))
    (nth-argument-type n (fdefinition symbol)))
  (:documentation
   "Return a type specifier that describes the set of permissible values
for the Nth argument of FUNCTION."))

(defgeneric argument-types (function)
  (:method ((symbol symbol))
    (argument-types (fdefinition symbol)))
  (:documentation
   "Return a list of the types of all mandatory arguments of FUNCTION."))

(defgeneric value-types (function)
  (:method ((symbol symbol))
    (values-types (fdefinition symbol)))
  (:documentation
   "Return a type specifier that describes the multiple values that will be
   returned when FUNCTION is called with valid arguments."))

(defgeneric function-type (function)
  (:method ((symbol symbol))
    (function-type (fdefinition symbol)))
  (:documentation
   "Return a type specifier describing FUNCTION."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod restrict :before ((function function) (argument-types list))
  (check-arity function (length argument-types)))

(defmethod restricted-function-p ((object t))
  nil)

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
