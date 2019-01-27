(in-package #:restricted-functions)

(defun restrict (function argument-types
                 &key cache strategy
                   (simplification #'identity))
  (unless (eq simplification #'identity)
    (setf argument-types (mapcar simplification argument-types)))
  (cond ((not cache) (%restrict function argument-types strategy))
        ((restricted-function-cache-value cache function argument-types strategy))
        ((setf (restricted-function-cache-value cache function argument-types strategy)
               (%restrict function argument-types strategy)))))

(defun %restrict (function argument-types strategy)
  (make-restricted-function
   function
   argument-types
   (infer-type function argument-types strategy)))
