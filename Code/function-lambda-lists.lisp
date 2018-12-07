(in-package :restricted-functions)

(defun function-lambda-list (function)
  "Return the lambda list of FUNCTION.  Signal an error if the
implementation has no means to determine the function's lambda list."
  (let ((arglist (trivial-arguments:arglist function)))
    (if (eq arglist :unknown)
        '(&rest anything)
        arglist)))

(defun lambda-list-arity (lambda-list)
  "Return two values:
   1. the number of mandatory arguments
   2. the maximal number of permissible arguments"
  (let ((mandatory-arguments 0)
        (max-arguments 0)
        (upper-bound-p t)
        (mandatory-increment 1)
        (max-increment 1))
    (declare (type (integer 0 #.call-arguments-limit)
                   mandatory-arguments max-arguments
                   mandatory-increment max-increment)
             (type boolean upper-bound-p))
    (dolist (item lambda-list)
      (case item
        ((&key)
         (setf max-increment 2)
         (setf mandatory-increment 0))
        ((&optional)
         (setf max-increment 1)
         (setf mandatory-increment 0))
        ((&aux)
         (setf max-increment 0)
         (setf mandatory-increment 0))
        ((&rest &allow-other-keys #+ccl ccl::&lexpr)
         (setf max-increment 0)
         (setf mandatory-increment 0)
         (setf upper-bound-p nil))
        (t
         (incf mandatory-arguments mandatory-increment)
         (incf max-arguments max-increment))))
    (if upper-bound-p
        (values mandatory-arguments max-arguments)
        (values mandatory-arguments call-arguments-limit))))

(defun check-arity (function number-of-supplied-arguments)
  (multiple-value-bind (mandatory-arguments max-arguments)
      (lambda-list-arity
       (function-lambda-list function))
    (unless (>= number-of-supplied-arguments mandatory-arguments)
      (error "~@<Only ~R argument~:P given for ~S, with ~
                 ~R mandatory argument~:P.~:@>"
             number-of-supplied-arguments
             function
             mandatory-arguments))
    (unless (<= number-of-supplied-arguments max-arguments)
      (error "~@<Received ~R argument~:P for ~S, which ~
                 expects ~:[at most~;exactly~] ~R argument~:P.~:@>"
             number-of-supplied-arguments
             function
             (= max-arguments mandatory-arguments)
             max-arguments))))
