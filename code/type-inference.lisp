(in-package :restricted-functions)

;;; To preserve everyone's sanity, this library only deals with a carefully
;;; selected subset of the types that are expressible in Common Lisp.  All
;;; other type-specifiers are either simplified to T, or NIL.

(defvar *type-hierarchy*
  '(t
    (number
     (integer u1 u2 u4 u8 u16 u32 u64 s1 s2 s4 s8 s16 s32 s64)
     (float f16 f32 f64)
     (complex c32 c64))
    function
    character
    package
    symbol
    cons))

(deftype u1 () 'bit)
(deftype u2 () '(unsigned-byte 2))
(deftype u4 () '(unsigned-byte 4))
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype u32 () '(unsigned-byte 32))
(deftype u64 () '(unsigned-byte 64))
(deftype s1 () '(signed-byte 1))
(deftype s2 () '(signed-byte 2))
(deftype s4 () '(signed-byte 4))
(deftype s8 () '(signed-byte 8))
(deftype s16 () '(signed-byte 16))
(deftype s32 () '(signed-byte 32))
(deftype s64 () '(signed-byte 64))

(defmacro define-float-types (bits)
  (flet ((float-bits (float)
           (+ (integer-length (nth-value 1 (decode-float float)))
              (float-precision float))))
    (let* ((floats (list most-positive-short-float
                         most-positive-single-float
                         most-positive-double-float
                         most-positive-long-float))
           (representant (or (find bits floats :key #'float-bits)
                             (error "No support for ~D bit floats." bits)))
           (float-type (class-name (class-of representant)))
           (complex-type `(complex ,float-type)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defconstant ,(intern (format nil "+F~D-TYPE-SYMBOL+" bits) :restricted-functions)
           ',float-type)
         (deftype ,(intern (format nil "F~D" bits) :restricted-functions) ()
           ',float-type)
         (deftype ,(intern (format nil "C~D" bits) :restricted-functions) ()
           ',complex-type)))))

(define-float-types 32)
(define-float-types 64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type Constructors

(defun make-unsigned-byte-type (n)
  (if (<= n 8)
      (if (<= n 2)
          (if (<= n 1) 'u1 'u2)
          (if (<= n 4) 'u4 'u8))
      (if (<= n 32)
          (if (<= n 16) 'u16 'u32)
          (if (<= n 64) 'u64 'integer))))

(defun make-signed-byte-type (n)
  (if (<= n 8)
      (if (<= n 2)
          (if (<= n 1) 's1 's2)
          (if (<= n 4) 's4 's8))
      (if (<= n 32)
          (if (<= n 16) 's16 's32)
          (if (<= n 64) 's64 'integer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simplification of Type Specifiers

(defun simplify-type-specifier (type-specifier)
  (flet ((fail () (error "Invalid type specifier: ~A" type-specifier)))
    (trivia:match type-specifier
      ;; Known symbols.
      ((and (type symbol)
            (or 't 'number 'integer 'float 'complex
                'u2 'u4 'u8 'u16 'u32 'u64
                's2 'i4 's8 's16 's32 's64
                'f32 'f64 'c32 'c64 'bit
                'function 'character 'package 'symbol 'cons))
       type-specifier)
      ;; Unsigned integer types.
      ((or 'unsigned-byte (list 'unsigned-byte '*))
       'integer)
      ((list 'unsigned-byte (and n (type (integer 1 *))))
       (make-unsigned-byte-type n))
      ;; Signed integer types.
      ((or 'signed-byte (list 'signed-byte '*))
       'integer)
      ((list 'signed-byte (and n (type (integer 1 *))))
       (make-signed-byte-type n))
      ;; Interval integer types.
      ((or 'integer
           (list 'integer)
           (list 'integer '*)
           (list 'integer (or '* (type integer)) '*)
           (list 'integer '* (or '* (type integer))))
       'integer)
      ((list 'integer (and lower-bound (type integer)) (and upper-bound (type integer)))
       (assert (<= lower-bound upper-bound))
       (if (minusp lower-bound)
           (make-signed-byte-type
            (max (integer-length lower-bound)
                 (integer-length upper-bound)))
           (make-unsigned-byte-type
            (integer-length upper-bound))))
      ;; Floating point types.
      ((list* '#.+f32-type-symbol+ _)
       'f32)
      ((list* '#.+f64-type-symbol+ _)
       'f64)
      ((or (list 'complex (or '#.+f32-type-symbol+ 'f32))
           (list 'complex (list* (or '#.+f32-type-symbol+ 'f32) _)))
       'c32)
      ((or (list 'complex (or '#.+f64-type-symbol+ 'f64))
           (list 'complex (list* (or '#.+f64-type-symbol+ 'f64) _)))
       'c64)
      ((list 'complex _)
       'complex)
      ;; Logical connectives.
      ((list 'or)
       'nil)
      ((list 'and)
       't)
      ((list (or 'and 'or) type)
       type)
      ((list* 'and types)
       (reduce #'type-and types))
      ((list* 'or types)
       (reduce #'type-or types))
      ;; Error handling.
      ((list* (or 'unsigned-byte 'signed-byte 'integer 'complex
                  '#.+f32-type-symbol+ '#.+f64-type-symbol+) _)
       (fail))
      ;; Expand recursively, or return T.
      (_
       (multiple-value-bind (expansion expanded-p)
           (introspect-environment:typexpand-1 type-specifier)
         (if (not expanded-p)
             't
             (simplify-type-specifier expansion)))))))

(defun type-and (t1 t2)
  ;; TODO
  t1)

(defun type-or (t1 t2)
  ;; TODO
  't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Obtaining the Simplified Type of an Object

(defgeneric simplified-type-of (object)
  (:documentation
   "Returns a simplified type specifier for a type that has OBJECT as an
element.

The expression (simplified-type-of OBJECT) yields exactly the same result
as the expression (simplify-type (type-of OBJECT)), but the former is
likely to be more efficient."))

(defmethod simplified-type-of ((object t))
  't)

(defmethod simplified-type-of ((integer integer))
  (let ((bits (integer-length integer)))
    (if (minusp integer)
        (make-signed-byte-type bits)
        (make-unsigned-byte-type bits))))

(defmethod simplified-type-of ((f32 #.+f32-type-symbol+))
  'f32)

(defmethod simplified-type-of ((f64 #.+f64-type-symbol+))
  'f64)

(defmethod simplified-type-of ((complex complex))
  (typecase complex
    ((complex #.+f32-type-symbol+) 'c32)
    ((complex #.+f64-type-symbol+) 'c64)
    (t (call-next-method))))

(defmethod simplified-type-of ((number number))
  'number)

(defmethod simplified-type-of ((function function))
  'function)

(defmethod simplified-type-of ((character character))
  'character)

(defmethod simplified-type-of ((package package))
  'package)

(defmethod simplified-type-of ((symbol symbol))
  'symbol)

(defmethod simplified-type-of ((cons cons))
  'cons)
