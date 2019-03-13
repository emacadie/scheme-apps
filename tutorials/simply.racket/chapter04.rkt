#lang simply-scheme
(se (butlast (bf "this"))
    "world")

(define (sphere-volume r)
  (* (/ 4 3) 3.141592654 (* r r r))) 

(module+ test
  (require rackunit)
  (check-true #t)
  (printf "(sphere-volume 10):  ~a \n" (sphere-volume 10)) 
  (check-equal? (sphere-volume 10) 4188.790205333334 "Wrong answer for sphere-volume" ) 
)



