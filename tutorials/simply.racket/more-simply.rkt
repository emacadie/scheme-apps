#lang simply-scheme

(provide base-grade
         display-all
         divisible?
         do-great-stuff
         modify-grade
         phone-letter
         plural
         simply-second
         square
         vowel?)

; (module more-simply simply-scheme)

(define (base-grade grade)
  (cond [(equal? (first grade) 'A) 4]
        [(equal? (first grade) 'B) 3]
        [(equal? (first grade) 'C) 2]
        [(equal? (first grade) 'D) 1]
        [else 0]))

(define (display-all . vs)
  (for-each display vs)
  (newline))

(define (divisible? big little)
  (= (remainder big little) 0))

(define (do-great-stuff the-word)
  (cond [(equal? the-word 'good) 'great]
        [(equal? the-word 'bad) 'terrible]
        [(number? the-word) (* 2 the-word)]
        [else the-word]))

(define (modify-grade grade)
  (cond [(equal? (last grade) '+)  0.33]
        [(equal? (last grade) '-) -0.33]
        [else 0]))

(define (phone-letter lttr)
  (cond [(member? lttr 'abc)  2]
        [(member? lttr 'def)  3]
        [(member? lttr 'ghi)  4]
        [(member? lttr 'jkl)  5]
        [(member? lttr 'mno)  6]
        [(member? lttr 'pqrs) 7]
        [(member? lttr 'tuv)  8]
        [(member? lttr 'wxyz) 9]
        [else 0]))

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




