(uiop:define-package :ga/converter
    (:use :cl)
  (:import-from :bit-smasher)
  (:export #:bit-vector->float-number
           #:float-number->bit-vector
           #:str->list
           #:*x-min*
           #:*x-max*
           #:*digit-capacity*))

(in-package :ga/converter)

(defvar *x-min*)
(defvar *x-max*)
(defvar *digit-capacity*)

;; to exports
(defun bit-vector->float-number (bit-vector)
  (+
   (* (bit-vector-to-integer bit-vector)
      (/ (- *x-max* *x-min*)
         (1- (expt 2 *digit-capacity*))))
   *x-min*))

(defun float-number->bit-vector (float-number)
  (integer-to-bit-vector
   (truncate
    (/
     (*
      (- float-number
         *x-min*)
      (1- (expt 2 *digit-capacity*)))
     (- *x-max* *x-min*)))))

;; utils
(defun bit-vector-to-integer (bit-vector)
  (let ((first-bit (aref bit-vector 0))
        (res (reduce
              (lambda (a b)
                (+ (ash a 1) b))
              (subseq bit-vector 1))))
    (if (eql first-bit 1)
        (* -1 res)
        res)))

(defun integer-to-bit-vector (integer)
  (labels ((%integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (%integer->bit-list i (push r accum))))
                   ((null accum)
                    (push 0 accum))
                   (t accum))))
    (let ((res (coerce
                (%integer->bit-list (if (< integer 0)
                                        (* -1 integer)
                                        integer))
                'bit-vector)))
      (concatenate 'bit-vector
                   (make-front-bit-vector res (< integer 0))
                   res))))

(defun make-front-bit-vector (bit-vector less-then-zero)
  (concatenate
   'bit-vector
   (list
    (if less-then-zero
        1
        0))
   (repeat-number->bit-vector
    (- (1- *digit-capacity*)
       (length bit-vector))
    0)))

(defun repeat-number->bit-vector (n number)
  (coerce (loop :repeat n :collect number) 'bit-vector))
