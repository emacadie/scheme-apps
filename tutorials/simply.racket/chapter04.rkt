#lang simply-scheme

(se (butlast (bf "this"))
    "world")

(define (sphere-volume r)
  (* (/ 4 3) 3.141592654 (* r r r))) 

;; 4.5: Write a procedure to convert a temperature from Fahrenheit to Celsius, 
;; and another to convert in the other direction. 
;; The two formulas are F=9⁄5C+32 and C=5⁄9(F-32).  
(define (c-to-f c)
  (exact->inexact (+ 32 (* (/ 9 5) c))))

(define (f-to-c f)
  (exact->inexact (* (/ 5 9) (- f 32))))

;; 4.6: Define a procedure fourth that computes the fourth power of its argument. 
;; Do this two ways, first using the multiplication function, and then using square and not (directly) using multiplication. 
(define (fourth x)
    (* x (* x (* x x))))

(define (square x)
  (* x x))

(define (fourth-with-square x)
  (square (square x)))

;; 4.7: Write a procedure that computes the absolute value of its argument by finding the square root of the square of the argument.
(define (simply-abs x)
  (sqrt (square x)))

;; 4.8  "Scientific notation" is a way to represent very small or very large numbers by combining a medium-sized number with a power of 10. 
;; For example, 5×107 represents the number 50000000, while 3.26×10-9 represents 0.00000000326 in scientific notation. 
;; Write a procedure scientific that takes two arguments, a number and an exponent of 10, and returns the corresponding value:
;; > (scientific 7 3)
;; 7000
;; > (scientific 42 -5)
;; 0.00042
;; Some versions of Scheme represent fractions in a/b form, and some use scientific notation, 
;; so you might see 21/50000 or 4.2E-4 as the result of the last example instead of 0.00042, but these are the same value.
(define (scientific num exp)
  (exact->inexact (* num (expt 10 exp))))

;; 4.9  Define a procedure discount that takes two arguments: an item's initial price and a percentage discount. It should return the new price:
;; > (discount 10 5)
;; 9.50
;; > (discount 29.90 50)
;; 14.95
(define (discount initial percent)
  (* initial (exact->inexact (/ (- 100 percent ) 100))))

;;  4.10  Write a procedure to compute the tip you should leave at a restaurant. 
;; It should take the total bill as its argument and return the amount of the tip. 
;; It should tip by 15%, but it should know to round up so that the total amount of money you leave (tip plus original bill) is a whole number of dollars. 
;; (Use the ceiling procedure to round up.)

;; > (tip 19.98)
;; 3.02 - for some reason I get 3.0199999999999996
;; > (tip 29.23)
;; 4.77
;; > (tip 7.54)
;; 1.46
;; 
(define (tip total-bill)
  (- (ceiling (* total-bill 1.15)) total-bill))

(module+ test
  (require rackunit)
  (check-true #t)
  (printf "(sphere-volume 10):  ~a \n" (sphere-volume 10)) 
  (check-equal? (sphere-volume 10) 4188.790205333334 "Error with: sphere-volume" ) 
  (printf "(c-to-f 23): ~a \n" (c-to-f 23))
  (check-equal? (c-to-f 23) 73.4 "Error with: (c-to-f 23)")
  (printf "(f-to-c 55): ~a \n" (f-to-c 55))
  (check-equal? (f-to-c 55) 12.777777777777779 "Error with: (f-to-c 55)")
  (printf "(fourth-with-square 4): ~a \n" (fourth-with-square 4))
  (check-equal? (fourth-with-square 4) 256 "Error with: (fourth-with-square 4)")
  (printf "(simply-abs -4): ~a \n" (simply-abs -4))
  (check-equal? (simply-abs -4) 4 "Error with: (simply-abs -4)")
  (printf "(scientific 7 3): ~a \n" (scientific 7 3))
  (check-equal? (scientific 7 3) 7000.0 "Error with: (scientific 7 3)")
  (printf "(scientific 42 -5): ~a \n" (scientific 42 -5))
  (check-equal? (scientific 42 -5) 0.00042 "Error with: (scientific 42 -5)")
  (printf "(discount 10 5): ~a \n" (discount 10 5))
  (check-equal? (discount 10 5) 9.5 "Error with: (discount 10 5)")
  (printf "(discount 29.90 50): ~a \n" (discount 29.90 50))
  (check-equal? (discount 29.90 50) 14.95 "Error with: (discount 29.90 50)")
  (printf "(tip 29.23): ~a \n" (tip 29.23))
  (check-equal? (tip 29.23) 4.77 "Error with: (tip 29.23)")
  (printf "(tip 7.54): ~a \n" (tip 7.54))
  (check-equal? (tip 7.54) 1.46 "Error with: (tip 7.54)")
)



