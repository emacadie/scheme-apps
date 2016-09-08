;; Using Scheme as a Calculator

;; exercise 1
;; (1+39) * (53-45)
(* (+ 1 39) (- 53 45)) ;; 320
;; (1020 / 39) + (45 * 2)
(+ (/ 1020  39) (* 45  2)) ;; 1510/13
;;Sum of 39, 48, 72, 23, and 91
(+ 39 48 72 23 91) ;; 273
(define numlist '(39 48 72 23 91))

    
;; someone made 'reduce' for scheme
;; http://stackoverflow.com/questions/26242546/defining-reduce-on-a-list-in-scheme
(define (reduce op lst)
  (let loop ((res (car lst)) (lst (cdr lst)))
    (if (null? lst)
        res
        (loop (op res (car lst)) (cdr lst)))))
;; Average of 39, 48, 72, 23, and 91 as a floating point. 
(define qq '(39 48 72 23 91))
(exact->inexact (/ (reduce + qq) (length qq))) ;; 54.6
(exact->inexact (/ (+ 39 48 72 23 91) (length '(39 48 72 23 91)))) ;; 54.6

(quotient 7 3) ;; 2
(modulo 7 3) ;; 1
(sqrt 8) ;; 2.8284271247461903
(sqrt 8.1) ;; 2.8460498941515415

;; trig functions
(atan 1) ;; 0.7853981633974483
(atan 1 0) ;; 1.5707963267948966

;; Exponential and logarithm are calculated by exp, and log, respectively. 
;; The value of b to the power of a can be calculated by (expt a b)

;; exercise 2
;; circle ratio, π
(exact->inexact (/ 22 7)) ;; 3.142857142857143
;; also
(* 4 (atan 1.0)) ;; 3.141592653589793 ;; more exact
;; exp(2/3)
(exp (/ 2 3)) ;; 1.9477340410546757
;; 3 to the power of 4
(expt 3 4) ;; 81
;; logarithm of 1000
(log 1000) ;; 6.907755278982137

