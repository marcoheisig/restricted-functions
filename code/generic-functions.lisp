(in-package #:restricted-functions)

(defgeneric infer-type (function argument-types strategy)
  (:documentation
   "Returns a values type specifier, describing the possible values of
FUNCTION when called with arguments of the given ARGUMENT-TYPES."))

(defgeneric name (function)
  (:documentation
   "Returns a symbol that is the name of FUNCTION."))

(defgeneric original-function (function)
  (:documentation
   "Returns the function upon which FUNCTION specializes."))

(defgeneric arity (function)
  (:documentation
   "Returns the number of arguments of FUNCTION."))

(defgeneric mandatory-values (function)
  (:documentation
   "Returns the number of mandatory values returned by FUNCTION."))

(defgeneric optional-values (function)
  (:documentation
   "Returns the number of optional values returned by FUNCTION."))

(defgeneric rest-values-p (function)
  (:documentation
   "Returns whether FUNCTION may return any number of values."))

(defgeneric nth-value-type (n function)
  (:documentation
   "Returns a type specifier that describes the Nth value returned by FUNCTION."))

(defgeneric nth-argument-type (n function)
  (:documentation
   "Returns a type specifier that describes the set of permissible values
for the Nth argument of FUNCTION."))

(defgeneric argument-types (function)
  (:documentation
   "Returns a list of the types of all mandatory arguments of FUNCTION."))

(defgeneric values-type (function)
  (:documentation
   "Returns a values type specifier that describes the multiple values that
will be returned when FUNCTION is called with valid arguments."))

(defgeneric function-type (function)
  (:documentation
   "Returns a type specifier describing FUNCTION."))