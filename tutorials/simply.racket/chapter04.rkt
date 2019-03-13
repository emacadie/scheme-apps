#lang simply-scheme
(se (butlast (bf "this"))
    "world")

(define (sphere-volume r)
  (* (/ 4 3) 3.141592654 (* r r r))) 

;; 4.5: Write a procedure to convert a temperature from Fahrenheit to Celsius, and another to convert in the other direction. The two formulas are F=9⁄5C+32 and C=5⁄9(F-32).  
(define (c-to-f c)
  (inexact (+ 32 (* (/ 9 5) c))))

(define (f-to-c f)
  (* (/ 5 9) (- f 32)))

(module+ test
  (require rackunit)
  (check-true #t)
  (printf "(sphere-volume 10):  ~a \n" (sphere-volume 10)) 
  (check-equal? (sphere-volume 10) 4188.790205333334 "Wrong answer for sphere-volume" ) 
)



