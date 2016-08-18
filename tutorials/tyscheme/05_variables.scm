;; 05 - lexical variables

;; variables have lexical scope
(define x 9)
(define add2 (lambda (x) (+ x 2))) ;; x in "add2" is lexically scoped; not the same as the global x
x ;; 9
(add2 3) ;; 5
(add2 x) ;; 11
x ;; still 9

(set! x 20) ;; change global c
x ;; 20
(define add2
    (lambda (x)
      (set! x (+ x 2)) ;; is this a lexical scope inside a lexical scope?
      x))
x ;; 20 (value unchanged)
(add2 x) ;; 22
x ;; 20 global x not changed by function

(define counter 0)
;; bump-counter will use global "counter" since none is defined in the lambda clause
(define bump-counter
    (lambda ()
      (set! counter (+ counter 1))
      counter))
counter ;; 0
(bump-counter) ;; 1
counter ;; 1
(bump-counter) ;; 2
(bump-counter) ;; 3
counter ;; 3

;; let and let*
;; "let" will create a variable for use within the body of "let"
(let ((x 1)
        (y 2)
        (z 3))
    (list x y z))
;; returns (1 2 3)

(let ((x 1)
        (y x))
    (+ x y))
;; returns 21, y is bound to global x

(let ((x 3)
        (y 8))
    (+ x y))
;; returns 11

;; let* will cause all variables to have lexical scope
(let* ((x 1)
       (y x))
    (+ x y))
;; returns 2

;; here is a more verbose version:
(let ((x 1))
    (let ((y x))
      (+ x y)))
;; also returns 2

;; let can bind variables to procedures
(let ((cons (lambda (x y) (+ x y))))
    (cons 1 2))
;; returns 3
;; I think it's just a convoluted way of making a "let" that adds numbers

;; fluid-let allows you to override global vars in a lexical scope
(display counter) ;; 3
(fluid-let ((counter 99))
    (display (bump-counter)) (newline)
    (display (bump-counter)) (newline)
    (display (bump-counter)) (newline))
;; returns: 
;; 100
;; 101
;; 102
(display counter) ;; still 3

;; back to regular old "let" to make a function that alters global variabl
(let ((counter 99))
    (display (bump-counter)) (newline)
    (display (bump-counter)) (newline)
    (display (bump-counter)) (newline))
;; 4
;; 5
;; 6
(display counter) ;; 6




