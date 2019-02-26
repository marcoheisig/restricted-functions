(in-package #:restricted-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLHS Figure 12-1 - Arithmetic Functions

(defun parse-number-type (type)
  (check-type type simplified-types:simplified-number-type-specifier)
  (if (consp type)
      (case (first type)
        (integer (values nil 'integer))
        (complex (values t (second type))))
      (values nil type)))

(defun unparse-number-type (complexp atomic-type)
  (if complexp
      (if (member atomic-type '(nil integer))
          't
          `(complex ,atomic-type))
      (if (eq atomic-type 'integer)
          '(integer * *)
          atomic-type)))

(defun numeric-contagion/atomic (type-1 type-2)
  (declare (type (or simplified-types:simplified-floating-point-type-specifier
                     (eql integer)
                     (eql t))
                 type-1 type-2))
  ;; The hierarchy is
  ;; integer < short-float < single-float < double-float < long-float < t
  (ecase type-1
    (integer type-2)
    (short-float (case type-2
                   ((integer) type-1)
                   (otherwise type-2)))
    (single-float (case type-2
                    ((integer short-float) type-1)
                    (otherwise type-2)))
    (double-float (case type-2
                    ((long-float t) type-2)
                    (otherwise type-1)))
    (long-float (case type-2
                  ((t) 't)
                  (otherwise type-1)))
    (t 't)))

(defun numeric-contagion (type-1 type-2)
  (multiple-value-bind (complexp-1 atomic-type-1)
      (parse-number-type type-1)
    (multiple-value-bind (complexp-2 atomic-type-2)
        (parse-number-type type-2)
      (unparse-number-type
       (or complexp-1 complexp-2)
       (numeric-contagion/atomic atomic-type-1 atomic-type-2)))))

(defun bounded-integer-type-p (type)
  (typep type '(cons (eql integer)
                (cons integer
                 (cons integer null)))))

(define-type-inference-rule + (&rest numbers)
  (if (null numbers)
      '(integer 0 0)
      (if (every #'bounded-integer-type-p numbers)
          (loop for (nil lb ub) in numbers
                sum lb into lower-bound
                sum ub into upper-bound
                finally
                   (return
                     `(integer ,lower-bound ,upper-bound)))
          (reduce #'numeric-contagion numbers))))

(define-type-inference-rule - (number &rest more-numbers)
  (if (null more-numbers)
      number
      (if (and (bounded-integer-type-p number)
               (every #'bounded-integer-type-p more-numbers))
          (loop for (nil lb ub) in more-numbers
                sum lb into min-subtraction
                sum ub into max-subtraction
                finally
                   (destructuring-bind (lb ub) (rest number)
                     (return
                       `(integer ,(- lb max-subtraction)
                                 ,(- ub min-subtraction)))))
          (reduce #'numeric-contagion more-numbers :initial-value number))))

(define-type-inference-rule * (&rest numbers)
  (if (null numbers)
      '(integer 1 1)
      (if (every #'bounded-integer-type-p numbers)
          (flet ((integer-type-* (type-1 type-2)
                   (destructuring-bind (lb-1 ub-1) (rest type-1)
                     (destructuring-bind (lb-2 ub-2) (rest type-2)
                       (let ((a (* lb-1 lb-2))
                             (b (* lb-1 ub-2))
                             (c (* ub-1 lb-2))
                             (d (* ub-1 ub-2)))
                         `(integer ,(min a b c d)
                                   ,(max a b c d)))))))
            (reduce #'integer-type-* numbers))
          (reduce #'numeric-contagion numbers))))

(define-type-inference-rule / (number &rest more-numbers)
  (if (and (bounded-integer-type-p number)
           (every #'bounded-integer-type-p more-numbers))
      't
      (reduce #'numeric-contagion more-numbers :initial-value number)))

(define-type-inference-rule 1+ (number)
  (if (bounded-integer-type-p number)
      (destructuring-bind (lb ub) (rest number)
        `(integer ,(1+ lb) ,(1+ ub)))
      (multiple-value-call #'unparse-number-type
        (parse-number-type number))))

(define-type-inference-rule 1- (number)
  (if (bounded-integer-type-p number)
      (destructuring-bind (lb ub) (rest number)
        `(integer ,(1- lb) ,(1- ub)))
      (multiple-value-call #'unparse-number-type
        (parse-number-type number))))

;;; TODO floor ceiling truncate round

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLHS Figure 12-2 - Exponentials, Logarithms and Trigonometry


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLHS Figure 12-3 - Numeric Comparison and Predication
