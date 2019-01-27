(in-package #:restricted-functions)

(defclass simplified-type-inference ()
  ((%assume-simplified-types
    :initarg :assume-simplified-types
    :reader assume-simplified-types))
  (:default-initargs :assume-simplified-types nil))

(defmethod infer-type :around
    ((function function)
     argument-types
     (strategy simplified-type-inference))
  (if (assume-simplified-types strategy)
      (call-next-method)
      (call-next-method
       function
       (mapcar #'simplified-types:simplify-type argument-types)
       strategy)))

(defparameter *simplified-type-inference-functions*
  (make-hash-table :test #'eq))

(defmacro simplified-type-inference-function (function)
  `(values (gethash ,function *simplified-type-inference-functions*)))

(defmethod infer-type
    ((function function)
     argument-types
     (strategy simplified-type-inference))
  (let ((s-t-i-f (simplified-type-inference-function function)))
    (if (not s-t-i-f)
        (call-next-method)
        (apply s-t-i-f argument-types))))

(defmacro define-type-inference-function (fname args &body body)
  `(setf (simplified-type-inference-function #',fname)
         (lambda ,args ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Arithmetic Functions

(defun parse-number-type (type)
  (check-type type simplified-types:simplified-number-type-specifier)
  (if (consp type)
      (case (first type)
        (integer (values nil nil))
        (complex (values t (second type))))
      (values nil type)))

(defun floating-point-contagion (fpt-1 fpt-2)
  (declare (simplified-types:simplified-floating-point-type-specifier fpt-1 fpt-2))
  (if (eq fpt-1 fpt-2)
      fpt-1
      (ecase fpt-1
        (short-float fpt-2)
        (single-float
         (case fpt-2
           ((or double-float long-float) fpt-2)
           (otherwise fpt-1)))
        (double-float
         (case fpt-2
           (long-float fpt-2)
           (otherwise fpt-1)))
        (long-float fpt-1))))

(defun numeric-contagion (type-1 type-2)
  (multiple-value-bind (complexp-1 fp-type-1)
      (parse-number-type type-1)
    (multiple-value-bind (complexp-2 fp-type-2)
        (parse-number-type type-2)
      (let ((complexp (or complexp-1 complexp-2))
            (fp-type (cond ((and fp-type-1 fp-type-2)
                            (floating-point-contagion fp-type-1 fp-type-2))
                           (fp-type-1)
                           (fp-type-2)
                           (t nil))))
        (if complexp
            (if (null fp-type)
                't
                `(complex ,fp-type))
            (if (null fp-type)
                '(integer * *)
                fp-type))))))

(defun simplified-integer-type-p (type)
  (typep type 'simplified-types:simplified-integer-type-specifier))

(define-type-inference-function + (&rest args)
  (if (null args)
      '(integer 0 0)
      (if (every #'simplified-integer-type-p args)
          (loop for (nil lb ub) in args
                sum lb into lower-bound
                sum ub into upper-bound
                finally
                   (return
                     `(integer ,lower-bound ,upper-bound)))
          (reduce #'numeric-contagion args))))
