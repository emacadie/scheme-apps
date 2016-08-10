;; Chapter 3: Forms
;; In Scheme, forms are programs
;; first element of form is the head
;; if first element is a procedure, the remaining elements are arguments to that procedure, and are evaluated
;; head may be a "special form"
;; begin causes its subforms to be evaluated in order
;; define creates a variable and sets the value
;; set! changes the binding of a variable

;; procedures
;; use lambda to create your own
(lambda (x) (+ x 2)) ;; #<procedure>
((lambda (x) (+ x 2)) 5) ;; 7
;; use "define" and "lambda" to create a function
(define add2
    (lambda (x) (+ x 2)))
(add2 2) ;; 4
(add2 3.4) ;; 5.4
;; add2 takes one arg, so it is unary

;; define our own function
(define area
    (lambda (length breadth)
      (* length breadth)))
(area 2 7) ;; 14
;; that seems a bit more verbose than Clojure with "defun"

;; "apply" can be used to call a procedure
(define x '(1 2 3))
(apply + x) ;; 6
(apply + 1 2 3 x) ;; 12
x ;; (1 2 3)

;; "apply" has an implicit "begin"
(define display3
    (lambda (arg1 arg2 arg3)
      (begin
        (display arg1)
        (display " ")
        (display arg2)
        (display " ")
        (display arg3)
        (newline))))
(display3 "Scheme" "is" "interesting") ;; Scheme is interesting
(display3 'scheme 'is 'cool) ;; scheme is cool

;; same as
(define display3
    (lambda (arg1 arg2 arg3)
        (display arg1)
        (display " ")
        (display arg2)
        (display " ")
        (display arg3)
        (newline)))
(display3 'function 'was 'changed) ;; function was changed



