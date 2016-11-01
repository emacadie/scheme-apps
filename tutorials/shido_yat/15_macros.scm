;; 15 Macros

;; assign '() to a variable
(define-syntax nil!
    (syntax-rules ()
        ((_ x)
            (set! x '()))))
;; says _ represents name of macro

(define a 1)
a ;; 1
(nil! a)
a ;; ()

;; when is part of r7rs, so let's make ours when2
(define-syntax when2
    (syntax-rules ()
        ((_ pred b1 ...)
            (if pred (begin b1 ...)))))
;; ... represents arbitrary number of expressions

(let ((i 0))
    (when2 (= i 0)
        (display "i == 0")
        (newline)))
;; i == 0
(let ((i 0))
    (when2 (= i 1)
        (display "i == 0")
        (newline)))
;; nothing happens
(define-syntax while
    (syntax-rules ()
        ((_ pred b1 ...)
            (let loop() (when pred b1 ... (loop))))))

(define-syntax for
    (syntax-rules ()
        ((_ (i from to) b1 ...)
            (let loop((i from))
                (when (< i to)
                    b1 ...
                    (loop (+ 1 i)))))))

(let ((i 0))
    (while (< i 10)
        (display i)
        (display #\space)
        (set! i (+ i 1))))
;; 0 1 2 3 4 5 6 7 8 9

(for (i 0 10)
    (display i)
    (display #\space))
;; 0 1 2 3 4 5 6 7 8 9

;; Exercise 1
;; Write a macro in that several expressions are evaluated when the predicate is false. (it is the opposite of the when.)
(define-syntax when-not
    (syntax-rules ()
        ((_ pred b1 ...)
            (if (not pred) (begin b1 ...)))))
(let ((i 0))
    (when-not (= i 1)
        (display "i not 1 but ")
        (display i)
        (newline)))
;; i not 1 but 0
(let ((i 0))
    (when-not (= i 0)
        (display "i not 1 but ")
        (display i)
        (newline)))
;; nothing happens
;; shido's answer
(define-syntax unless
  (syntax-rules ()
    ((_ pred b1 ...)
     (if (not pred)
	 (begin
	   b1 ...)))))

;; 3 more about syntax-rules
;; syntax rules can take more than one pattern
;; here is a macro to increment a variable either by a given value, or by one if no value given
(define-syntax incf
    (syntax-rules ()
        ((_ x) (begin (set! x (+ x 1)) x))
        ((_ x i) (begin (set! x (+ x i)) x))))

(let ((i 0) (j 0))
    (incf i)
    (incf j 3)
    (display (list 'i '= i))
    (newline)
    (display (list 'j '= j)))
;; gives
;; (i = 1)
;; (j = 3)

;; Exercise 2
;; Write a macro decf that subtracts a value from the variable. If the decrement value is omitted, it subtracts one form the variable. 
(define-syntax decf
    (syntax-rules ()
        ((_ x) (begin (set! x (- x 1)) x))
        ((_ x i) (begin (set! x (- x i)) x))))
(let ((i 10) (j 10))
    (decf i)
    (decf j 3)
    (display (list 'i '= i))
    (newline)
    (display (list 'j '= j)))
;; gives:
;; (i = 9)
;; (j = 7)

;; shido's answer
(define-syntax decf
  (syntax-rules ()
    ((_ x) (begin (set! x (- x 1)) x))
    ((_ x i) (begin (set! x (- x i)) x))))

;;  Exercise 3
;; Improve the for shown in the [code 3] to accept a step width. If the step width is omitted, it is 1. 
(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (+ 1 i)))))))

(define-syntax for-step
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (+ 1 i)))))
	((_ (i from to step) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (+ step i)))))))
	
(for-step (i 0 10)
  (display i)
  (display #\Space))
;; 0 1 2 3 4 5 6 7 8 9
(for-step (i 0 10 2)
  (display i)
  (display #\Space))
;; 0 2 4 6 8 
;; shido's answer
(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (1+ i)))))
    ((_ (i from to step) b1 ...)
     (let loop ((i from))
       (when (< i to)
	  b1 ...
	  (loop (+ i step)))))))



