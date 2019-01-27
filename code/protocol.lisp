(in-package #:restricted-functions)

(defun restrict (function argument-types &optional strategy)
  (restrict-using-strategy function argument-types strategy))

(defgeneric restrict-using-strategy (function argument-types strategy)
  (:method ((symbol symbol) (argument-types list) strategy)
    (restrict (fdefinition symbol) argument-types strategy))
  (:documentation
   "Returns a function with the same semantics as FUNCTION, but whose
domain is restricted to arguments of the given ARGUMENT-TYPES.  If
possible, the result is a subclass of RESTRICTED-FUNCTION and can be
queried for a variety of information."))

(defgeneric function-name (function)
  (:method ((symbol symbol))
    (function-name (fdefinition symbol)))
  (:documentation
   "Returns a symbol that is the name of FUNCTION."))

(defgeneric original-function-name (function)
  (:method ((symbol symbol))
    (original-function-name (fdefinition symbol)))
  (:documentation
   "Returns the name of the function upon which FUNCTION specializes."))

(defgeneric arity (function)
  (:method ((symbol symbol))
    (arity (fdefinition symbol)))
  (:documentation
   "Returns the number of arguments of FUNCTION."))

(defgeneric minimum-number-of-values (function)
  (:method ((symbol symbol))
    (minimum-number-of-values (fdefinition symbol)))
  (:documentation
   "Returns an inclusive lower bound for the number of values returned by FUNCTION."))

(defgeneric maximum-number-of-values (function)
  (:method ((symbol symbol))
    (maximum-number-of-values (fdefinition symbol)))
  (:documentation
   "Returns an inclusive upper bound for the number of values returned by FUNCTION."))

(defgeneric nth-value-type (n function)
  (:method ((n integer) (symbol symbol))
    (nth-value-type n (fdefinition symbol)))
  (:documentation
   "Returns a type specifier that describes the Nth value returned by the
function described by RESTRICTED-FUNCTION."))

(defgeneric nth-argument-type (n function)
  (:method ((n integer) (symbol symbol))
    (nth-argument-type n (fdefinition symbol)))
  (:documentation
   "Returns a type specifier that describes the set of permissible values
for the Nth argument of FUNCTION."))

(defgeneric argument-types (function)
  (:method ((symbol symbol))
    (argument-types (fdefinition symbol)))
  (:documentation
   "Returns a list of the types of all mandatory arguments of FUNCTION."))

(defgeneric value-types (function)
  (:method ((symbol symbol))
    (values-types (fdefinition symbol)))
  (:documentation
   "Returns a type specifier that describes the multiple values that will be
   returned when FUNCTION is called with valid arguments."))

(defgeneric function-type (function)
  (:method ((symbol symbol))
    (function-type (fdefinition symbol)))
  (:documentation
   "Returns a type specifier describing FUNCTION."))
