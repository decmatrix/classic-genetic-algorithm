(uiop:define-package :ga/converter
    (:use :cl)
  (:exports #:bin-str->float-number
            #:float-number->bin-str
            #:str->list
            #:*x-min*
            #:*x-max*
            #:*digit-capacity*))

(in-package :ga/converter)

(defvar *x-min*)
(defvar *x-max*)
(defvar *digit-capacity*)

(defun bin-str->float-number (bin-str)
  (let ((num (bin->num bin-str)))
    (+
     (* num
        (/ (- *x-max* *x-min*)
           (1- (expt 2 *digit-capacity*))))
     *x-min*)))

(defun float-number->bin-str (float-number)
  (/
   (*
    (- float-number
       *x-min*)
    (-1 (expt 2 *digit-capacity*)))
   (- *x-max* *x-min*)))

(defun bin->num (bin)
  (parse-integer bin :radix 2))

(defun num->bin (num)
  (let ((res (write-to-string num :base 2)))
    (concatenate 'string
                 (repeat-chars->string
                  (- *digit-capacity* (length res))
                  #\0)
                 res)))

(defun repeat-chars->string (n ch)
  (coerce (loop :repeat n :collect ch) 'string))

(defun str->list (str)
  (loop :for i :from 0 :to (length str)
     :collect (aref str i)))
