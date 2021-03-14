(uiop:define-package :ga/ga-impl
    (:use :cl
          :ga/converter)
  (:nicknames :ga)
  (:exports))

(in-package :ga/ga-impl)

(defvar *count-of-genes*)
(defvar *fn*)
(defvar *count-of-elits*)
(defvar *count-of-tournament-chs*)

(defclass chromosome ()
  ((genes
    :initarg :genes
    :type list
    :reader get-genes)
   (phenotype
    :type float
    :writer set-phenotype
    :reader get-phenotype)))

(defmethod print-object ((obj chromosome) stream)
  (with-slots (phenotype genes)
      obj
    (print-unreadable-object
        (obj stream)
      (format stream "chromosome: phenotype - ~A, genes - ~{~a~}"
              phenotype
              genes))))

(defun generate-chromosome ()
  (make-instance 'chromosome
                 :genes (generate-genes)))

(defun generate-genes ()
  (loop :repeat *count-of-genes*
     :collect (float-number->bin-str
               (random (abs (- *x-min*
                               *x-max*))))))

(defun generate-population (count-of-pupulation)
  (loop :repeat count-of-pupulation
     :collect (generate-chromosome)))

;; ga process (search extremum of function)
(defun search-extremum-of-function (fn &key
                                         count-of-elits
                                         size-of-population
                                         x-min
                                         x-max
                                         digit-capacity
                                         count-of-genes
                                         count-of-iterations)
  (let* ((*count-of-elits* count-of-elits)
         (*count-of-tournament-chs* (- count-of-pupulation
                                       *count-of-elits*))
         (*fn* fn)
         (*x-min* x-min)
         (*x-max* x-max)
         (*digit-capacity* digit-capacity))
    (sort-population
     (loop :repeat count-of-iterations
        :for new-population (calculate-phenotypes
                             (generate-population size-of-population))
        :then (progn
                (calculate-phenotypes new-population)
                new-population)
        :do (setf new-population (selection-and-mutation new-population))
        :finally (return
                   (progn
                     (calculate-phenotypes new-population)
                     (car (sort-population new-population))))))))

(defun calculate-phenotypes (population)
  (mapc
   (lambda (ch)
     (setf (set-phenotype ch)
           (funcall *fn* (get-genes ch))))))

(defun sort-population (population)
  (sort :test #'< :key #'get-phenotype))

(defun init-population (n)
  (loop :repeat n :collect (generate-chromosome)))

;; crosovver operation
(defun two-point-crossover ()
  )


;; selections and mutations
(defun selections-and-mutations (population)
  )

(defun elitism-selection (population)
  (subseq population 0 *count-of-elits*))

(defun tournament-selection (population)
  (loop :repeat *count-of-tournament-chs*
     :collect (let ((ch1 (nth (random *count-of-tournament-chs*)
                              population))
                    (ch2 (nth (random *count-of-tournament-chs*)
                              population)))
                (if (< (get-phenotype ch1)
                       (get-phenotype ch2))
                    ch1
                    ch2))))

(defun mutation ()
  )
