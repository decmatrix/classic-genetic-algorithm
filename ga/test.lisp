(uiop:define-package :ga/test
    (:use :cl
          :ga/ga-impl
          :ga/functions)
  (:nicknames :ga-test)
  (:export #:test-all
           #:test-sphere-fn
           #:test-ackly-fn
           #:test-griewank-fn
           #:test-rastrigin-fn
           #:test-rosenbrock-fn ))

(in-package :ga/test)

(defun test-all ()
  (format t "Sphere fn: ~a~%"
          (test-sphere-fn))
  (format t "Ackly fn: ~a~%"
          (test-ackly-fn))
  (format t "Griewank fn: ~a~%"
          (test-griewank-fn))
  (format t "Rastrigin fn: ~a~%"
          (test-rastrigin-fn))
  (format t "Rosenbrock fn: ~a~%"
          (test-rosenbrock-fn)))

(defun test-sphere-fn ()
  (time (search-extremum-of-function
         #'sphere-fn
         :x-min -100.0
         :x-max +100.0)))

(defun test-ackly-fn ()
  (time (search-extremum-of-function
         #'ackley-fn
         :x-min -32.768
         :x-max +32.768)))

(defun test-griewank-fn ()
  (time (search-extremum-of-function
         #'griewank-fn
         :x-min -600.0
         :x-max 600.0)))

(defun test-rastrigin-fn ()
  (time (search-extremum-of-function
         #'rastrigin-fn
         :x-min -5.12
         :x-max 5.12)))

(defun test-rosenbrock-fn ()
  (time (search-extremum-of-function
         #'rosenbrock-fn
         :x-min -5.0
         :x-max 10.0)))
