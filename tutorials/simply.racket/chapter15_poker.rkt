#lang simply-scheme

; Chapter 15 project: Poker

(require (prefix-in more: "more-simply.rkt"))

(butfirst '(This is chapter 15 poker))

;; from chapter 9 bridge:
; Write a procedure count-suit that takes a suit and a hand as arguments and returns the number of cards in the hand with the given suit.
(define (count-suit suit card-list)
  (count (keep (lambda (x) (equal? suit x)) (every first card-list))))

;; Write a procedure suit-counts that takes a hand as its argument and returns a sentence containing the number of spades, 
;; the number of hearts, the number of clubs, and the number of diamonds in the hand.
(define (suit-counts suit-sentence)
  (every (lambda (x) (count-suit x suit-sentence)) '(c d h s)))

(define (num-clubs s-count)
  (first s-count))

(define (num-diamonds s-count)
  (more:simply-second s-count))

(define (num-hearts s-count)
  (last (butlast s-count)))

(define (num-spades s-count)
  (last s-count))

(define (count-rank rank card-list)
  (count (keep (lambda (x) (equal? rank x)) (every butfirst card-list))))

(define (rank-counts rank-sentence)
  (every (lambda (x) (count-rank x rank-sentence)) '(a 2 3 4 5 6 7 8 9 10 j q k)))

(define (check-flush card-list)
  (check-flush-work (suit-counts card-list)))

(define (check-flush-work list-suit-counts)
  (cond [(equal? 0 (appearances 5 list-suit-counts))      'none]
        [(equal? 5 (first list-suit-counts))              'clubs]
        [(equal? 5 (more:simply-second list-suit-counts)) 'diamonds]
        [(equal? 5 (last (butlast list-suit-counts)))     'hearts]
        [(equal? 5 (last list-suit-counts))               'spades]))

(module+ test
  (require rackunit)
  (check-true #t)

  (printf "(count-suit 's '(sa s10 hq ck c4)): ~a \n" (count-suit 's '(sa s10 hq ck c4)))
  (check-equal? (count-suit 's '(sa s10 hq ck c4)) 
                2 
                "Error for: (count-suit 's '(sa s10 hq ck c4))")
  (printf "(count-suit 'c '(c3 d8 dj c10 d5)): ~a \n" 
          (count-suit 'c '(c3 d8 dj c10 d5)))
  (check-equal? (count-suit 'c '(c3 d8 dj c10 d5)) 
                2 
                "Error for: (count-suit 'c '(c3 d8 dj c10 d5))")
  (printf "(count-suit 'd '(c3 d8 dj c10 d5)): ~a \n" (count-suit 'd '(c3 d8 dj c10 d5)))
  (check-equal? (count-suit 'd '(c3 d8 dj c10 d5)) 
                3 
                "Error for: (count-suit 'd '(c3 d8 dj c10 d5))")
   
  (printf "(num-clubs (suit-counts '(s4 s5 s9 c4 h6 h5))): ~a \n" (num-clubs (suit-counts '(s4 s5 s9 c4 h6 h5))))
  (check-equal? (num-clubs (suit-counts '(s4 s5 s9 c4 h6 h5))) 1 "Error for (num-clubs (suit-counts '(s4 s5 s9 c4 h6 h5)))")

(printf "(num-diamonds (suit-counts '(s4 s5 s9 c4 h6 h5))): ~a \n" (num-diamonds (suit-counts '(s4 s5 s9 c4 h6 h5))))
(check-equal? (num-diamonds (suit-counts '(s4 s5 s9 c4 h6 h5))) 0 "Error for (num-diamonds (suit-counts '(s4 s5 s9 c4 h6 h5)))")

(printf "(num-hearts (suit-counts '(s4 s5 s9 c4 h6 h5))): ~a \n" (num-hearts (suit-counts '(s4 s5 s9 c4 h6 h5))))
(check-equal? (num-hearts (suit-counts '(s4 s5 s9 c4 h6 h5))) 2 "Error for (num-hearts (suit-counts '(s4 s5 s9 c4 h6 h5)))")

(printf "(num-spades (suit-counts '(s4 s5 s9 c4 h6 h5))): ~a \n" (num-spades (suit-counts '(s4 s5 s9 c4 h6 h5))))
(check-equal? (num-spades (suit-counts '(s4 s5 s9 c4 h6 h5))) 3 "Error for (num-spades (suit-counts '(s4 s5 s9 c4 h6 h5)))")

;; hands that we can use
(printf "(suit-counts '(c3 d8 dj c10 d5)): ~a\n" (suit-counts '(c3 d8 dj c10 d5)))
(check-equal? (suit-counts '(c3 d8 dj c10 d5)) '(2 3 0 0) "Error for (suit-counts '(c3 d8 dj c10 d5))")
(printf "(suit-counts '(s5 cj ca h2 h7)): ~a\n" (suit-counts '(s5 cj ca h2 h7))
)
(check-equal? (suit-counts '(s5 cj ca h2 h7))  '(2 0 2 1) "Error for (suit-counts '(s5 cj ca h2 h7))")

(printf "(suit-counts '(h8 d4 d10 c10 ha)): ~a\n" (suit-counts '(h8 d4 d10 c10 ha)))
(check-equal? (suit-counts '(h8 d4 d10 c10 ha))  '(1 2 2 0) "Error for (suit-counts '(h8 d4 d10 c10 ha))")

  ; (printf ": ~a \n" )
  ; (check-equal?  "Error for: ")

) ;; end module+ test 
  ; (printf ": ~a \n"  )
; (check-equal?  "Error for: ")

