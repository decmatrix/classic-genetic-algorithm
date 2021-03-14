(uiop:define-package :ga/functions
    (:use :cl)
  (:nicknames :ga-functions)
  (:exports #:rosenbrock-fn
            #:rastrigin-fn
            #:griewank-fn
            #:ackley-fn
            #:sphere-fn))

(in-package :ga/functions)

;; SPHERE FUNCTION
(defun sphere-fn (x-lst)
  (loop :for x :across x-lst
     :summing (expt x 2)))

;; ACKLEY FUNCTION
(defun ackley-fn (x-lst)
  (+ (- (* -20
           (exp (* -0.2
                   (sqrt
                    (/ (loop :for x :across x-lst
                          :summing (expt x 2))
                       (length x-lst))))))
        (exp (/ (loop :for x :across x-lst
                   :summing (cos (* 2 pi x)))
                (length x-lst))))
     20
     (exp 1)))

;; GRIEWANK FUNCTION
(defun griewank-fn (x-lst)
  (1+
   (- (/ (loop :for x :across x-lst
            :summing (expt x 2))
         4000)
      (loop :for x :across x-lst
         :and i = 1 :then (1+ i)
         :for res = 1
         :then (* res
                  (cos (/ x (sqrt i))))
         :finally (return res)))))

;; RASTRIGIN FUNCTION
(defun rastrigin-fn (x-lst)
  (+ (* 10
        (length x-lst))
     (loop :for x :across x-lst
        :summing (- (expt x 2)
                    (* 10
                       (cos (* 2 pi x)))))))

;; ROSENBROCK FUNCTION
(defun rosenbrock-fn (x-lst)
  (loop :for i :to (length x-lst)
     :summing (+ (* 100
                    (expt (- (nth i x-lst)
                             (expt (nth (1- i) x-lst)
                                   2))
                          2))
                 (expt (1- (nth (1- i) x-lst))
                       2))))
