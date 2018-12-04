(in-package :restricted-functions)

(defrestrictor + (original-name argument-types)
  (make-instance 'simple-restricted-function
    :original-name original-name
    :argument-types argument-types
    :min-values 1
    :max-values 1
    :value-types 't))
