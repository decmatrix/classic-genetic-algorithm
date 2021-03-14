(uiop:define-package :ga/ga-impl
    (:use :cl
          :ga/converter)
  (:import-from :alexandria
                #:flatten)
  (:nicknames :ga)
  (:exports #:search-extremum-of-function))

(in-package :ga/ga-impl)

;; dynamic vars
(defvar *count-of-genes*)
(defvar *fn*)
(defvar *count-of-elits*)
(defvar *count-of-tournament-chs*)

;; CLOS: class of chromosome
(defclass chromosome ()
  ((genes
    :initarg :genes
    :type list
    :reader get-genes)
   (phenotype
    :type float
    :writer set-phenotype
    :reader get-phenotype)))

;; object printer
(defmethod print-object ((obj chromosome) stream)
  (with-slots (phenotype genes)
      obj
    (print-unreadable-object
        (obj stream)
      (format stream "chromosome: phenotype - ~A, genes - ~{~a~}"
              phenotype
              genes))))

;; generator
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
                                         (count-of-elits 5)
                                         (size-of-population 30)
                                         x-min
                                         x-max
                                         (digit-capacity 16)
                                         (count-of-genes 4)
                                         (count-of-iteration 5))
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
                (sort-population new-population))
        :do (setf new-population (selection-and-mutation new-population))
        :finally (return
                   (progn
                     (calculate-phenotypes new-population)
                     (car (sort-population new-population))))))))


;; population utils
(defun calculate-phenotypes (population)
  (mapc
   (lambda (ch)
     (setf (set-phenotype ch)
           (funcall *fn*
                    (mapcar #'bin-str->float-number
                            (get-genes ch)))))
   population))

(defun sort-population (population)
  (sort :test #'< :key #'get-phenotype))

(defun init-population (n)
  (loop :repeat n :collect (generate-chromosome)))

;; crosovver operation
(defun two-point-crossover (population)
  (flatten
   (loop :repeat *count-of-tournament-chs*
      :collect (crossover-swap
                (get-genes
                 (nth (radnom *count-of-genes*)
                      population))
                (get-genes
                 (nth (random *count-of-genes*)
                      population))))))

(defun crossover-swap (genes-of-ch1 genes-of-ch2)
  (let ((random-gen-1 (nth (random (length genes-of-ch1))
                           genes-of-ch1))
        (random-gen-2 (nth (random (length genes-of-ch2))
                           genes-of-ch2))
        (res (mapcar
              (lambda (gen-of-ch1 gen-of-ch2)
                (if (or (equal gen-of-ch1 random-gen-1)
                        (equal gen-of-ch2 random-gen-2))
                    (let ((bit-str-1 (str->list gen-of-ch1))
                          (bit-str-2 (str->list gen-of-ch2)))
                      (list
                       (coerce 'string
                               (append
                                (subseq bit-str-2 0 2)
                                (subseq bit-str-1 3)))
                       (coerce 'string
                               (append
                                (subseq bit-str-1 0 2)
                                (subseq bit-str-2 3)))))
                    (list gen-of-ch2 geb-of-ch1)))
              genes-of-ch1
              genes-of-ch2))
        (list
         (make-instance
          'chromosome
          :genes (mapcar #'first res))
          (make-instance
           'chromosome
           :genes (mapcar #'second res))))))

;; selections and mutations
(defun selection-and-mutation (population)
  (append
   (elitism-selection population)
   (mutation
    (two-point-crossover
     (tournament-selection population)))))

;; selections
(defun elitism-selection (population)
  (subseq population 0 *count-of-elits*))

(defun tournament-selection (population)
  (let ((population (subseq population *count-of-elits*)))
    (loop :repeat *count-of-tournament-chs*
       :collect (let ((ch1 (nth (random *count-of-tournament-chs*)
                                population))
                      (ch2 (nth (random *count-of-tournament-chs*)
                                population)))
                  (if (< (get-phenotype ch1)
                         (get-phenotype ch2))
                      ch1
                      ch2)))))

;; mutation
(defun mutation (population)
  (dolist (ch population)
    (make-instance
     'chromosome
     :genes (bits-mutation
             (get-genes ch)))))

(defun calculate-mutation-probability ()
  (*
   (/ 1
      *digit-capacity*)
   100))

(defun bits-mutation (bit-str)
  (let ((mutation-prob calculate-mutation-probability))
    (coerce
     'string
     (mapcar
      (lambda (bit)
        (if (> mutation-prob (random 100))
            (case bit
              (#\- #\+)
              (#\0 #\1)
              (#\1 #\0)
              (t (error "Unknown symbol: ~a" bit)))
            bit))
      (str->list bit-str)))))
