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
    (*
     (- float-number
        *x-min*)
     (/
      (1- (expt 2 *digit-capacity*))
      (- *x-max* *x-min*))))))

;; utils
(defun bit-vector-to-integer (bit-vector)
  (unless (eql (length bit-vector) *digit-capacity*)
    (error "wrong bit vector: ~a" bit-vector))
  (reduce
   (lambda (a b)
     (+ (ash a 1) b))
   bit-vector))

(defun integer-to-bit-vector (integer)
  (labels ((%integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (%integer->bit-list i (push r accum))))
                   ((null accum)
                    (push 0 accum))
                   (t accum))))
    (let* ((res (coerce
                 (%integer->bit-list (if (< integer 0)
                                         (* -1 integer)
                                         integer))
                 'bit-vector))
           (res-norm (make-front-bit-vector res)))
      (unless (eql *digit-capacity* (length res-norm))
        (error "wrong bit vector: ~a" res-norm))
      res-norm)))

(defun make-front-bit-vector (bit-vector)
  (concatenate
   'bit-vector
   (repeat-number->bit-vector
    (- *digit-capacity*
       (length bit-vector))
    0)
   bit-vector))

(defun repeat-number->bit-vector (n number)
  (coerce (loop :repeat n :collect number) 'bit-vector))
