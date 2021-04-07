#lang racket

;; quadratic equation

(define (quad-plus a b c) 
  (/ (+ (- b) 
        (sqrt (- (* b b) 
                 (* 4 a c)))) 
     (* 2 a))
) 

;; (define (quad-plus-ii a b c) 
;;   (/ (+ minus-b  ) 
;;      (* 2 a))
;; ) 

(module+ test
  (require rackunit)
  (check-true #t)
  ; (printf "(sphere-volume 10):  ~a \n" (sphere-volume 10)) 
  ; (check-equal? (sphere-volume 10) 4188.790205333334 "Error with: sphere-volume" )
  (check-equal? (quad-plus 1 2 3) -1.0+1.4142135623730951i "Error with: quad-plus" )
  (check-equal? (quad-plus 3 2 1) -0.3333333333333333+0.47140452079103173i "Error with: quad-plus" )
  
)
