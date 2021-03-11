(uiop:define-package :ga/ga-impl
    (:use :cl)
  (:exports))

(in-package :ga/ga-impl)

(defclass chromosome ()
  ((genes
    :initarg :genes
    :type list
    :reader get-genes)))


