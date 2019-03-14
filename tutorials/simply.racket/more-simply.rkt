#lang simply-scheme

(provide divisible?
         simply-second)

; (module more-simply simply-scheme)

(define (simply-second thing)
  (first (butfirst thing)))

(define (divisible? big little)
  (= (remainder big little) 0))


