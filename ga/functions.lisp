(uiop:define-package :ga/functions
    (:use :cl)
  (:nicknames :ga-functions)
  (:export #:rosenbrock-fn
           #:rastrigin-fn
           #:griewank-fn
           #:ackley-fn
           #:sphere-fn))

(in-package :ga/functions)

;; SPHERE FUNCTION
(defun sphere-fn (x-lst)
  (loop :for i :from 0 :to (1- (length x-lst))
     :summing (expt (nth i x-lst)
                    2)))

;; ACKLEY FUNCTION
(defun ackley-fn (x-lst)
  (+ (- (* -20
           (exp (* -0.2
                   (sqrt
                    (/ (loop :for i :from 0 :to (1- (length x-lst))
                          :summing (expt (nth i x-lst)
                                         2))
                       (length x-lst))))))
        (exp (/ (loop :for i :from 0 :to (1- (length x-lst))
                   :summing (cos (* 2
                                    pi
                                    (nth i x-lst))))
                (length x-lst))))
     20
     (exp 1)))

;; GRIEWANK FUNCTION
(defun griewank-fn (x-lst)
  (1+
   (- (/ (loop :for i :from 0 :to (1- (length x-lst))
            :summing (expt (nth i x-lst)
                           2))
         4000)
      (loop :for i :from 0 :to (1- (length x-lst))
         :and j = 1 :then (1+ j)
         :for res = 1
         :then (* res
                  (cos (/ (nth i x-lst) (sqrt j))))
         :finally (return res)))))

;; RASTRIGIN FUNCTION
(defun rastrigin-fn (x-lst)
  (+ (* 10
        (length x-lst))
     (loop :for i :from 0 :to (1- (length x-lst))
        :summing (- (expt (nth i x-lst)
                          2)
                    (* 10
                       (cos (* 2
                               pi
                               (nth i x-lst))))))))

;; ROSENBROCK FUNCTION
(defun rosenbrock-fn (x-lst)
  (loop :for i :from 1 :to (1- (length x-lst))
     :summing (+ (* 100
                    (expt (- (nth i x-lst)
                             (expt (nth (1- i) x-lst)
                                   2))
                          2))
                 (expt (1- (nth (1- i) x-lst))
                       2))))
