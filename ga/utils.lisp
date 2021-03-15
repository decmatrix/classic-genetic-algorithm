(uiop:define-package :ga/utils
    (:use :cl)
  (:nicknames :ga-utils)
  (:export #:random-from-range))

(in-package :ga/utils)

(defun random-from-range (start end)
  (let ((res (+ start (random (+ 1 (- end start))))))
    (if (or (> res end) (< res start))
        (random-from-range start end)
        res)))
