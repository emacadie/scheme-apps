#lang simply-scheme

(provide divisible?
         plural
         simply-second
         square
         vowel?)

; (module more-simply simply-scheme)

(define (divisible? big little)
  (= (remainder big little) 0))

(define (plural-y wd)
  (cond [(vowel? (last (butlast wd))) (word wd 's)]
        [else (word (butlast wd) 'ies)]))

(define (plural wd)
  (cond [(not (word? wd)) word]
        [(equal? (last wd) 'y) (plural-y wd)]
        [(equal? (last wd) 'x) (word wd 'es)]
        [else (word wd 's)]))

(define (simply-second thing)
  (first (butfirst thing)))

(define (square x)
  (* x x))

(define (vowel? arg)
   (cond [(or (equal? arg "a") (equal? arg "e") (equal? arg "i") (equal? arg "o") (equal? arg "u")) #t]
         [(or (equal? arg "A") (equal? arg "E") (equal? arg "I") (equal? arg "O") (equal? arg "U")) #t]
       [else #f]))




