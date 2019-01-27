(in-package #:restricted-functions)

(defvar *default-type-inference-strategy*
  (make-instance 'simplified-type-inference))

(defun restrict (function argument-types
                 &key cache
                   (strategy *default-type-inference-strategy*)
                   (key #'identity))
  (unless (eq key #'identity)
    (setf argument-types (mapcar key argument-types)))
  (cond ((not cache) (%restrict function argument-types strategy))
        ((restricted-function-cache-value cache function argument-types strategy))
        ((setf (restricted-function-cache-value cache function argument-types strategy)
               (%restrict function argument-types strategy)))))

(defun %restrict (function argument-types strategy)
  (make-restricted-function
   function
   argument-types
   (infer-type function argument-types strategy)))
