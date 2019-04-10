#lang simply-scheme

; Chapter 15 project: Poker

(require "more-simply.rkt")
; (require "simply-constants.rkt")
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
  (simply-second s-count))

(define (num-hearts s-count)
  (last (butlast s-count)))

(define (num-spades s-count)
  (last s-count))

(define (count-rank rank card-list)
  (count (keep (lambda (x) (equal? rank x)) (every butfirst card-list))))

(define (rank-counts rank-sentence)
  (every (lambda (x) (count-rank x rank-sentence)) '(a 2 3 4 5 6 7 8 9 10 j q k a)))

(module+ test
  (require rackunit)
  (check-true #t)

  (printf "(count-suit 's '(sa s10 hq ck c4)): ~a \n" (count-suit 's '(sa s10 hq ck c4)))
  (check-equal? (count-suit 's '(sa s10 hq ck c4)) 
                2 
                "Error for: (count-suit 's '(sa s10 hq ck c4))")
  (printf "(count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)): ~a \n" 
          (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) )
  (check-equal? (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) 
                2 
                "Error for: (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))")
  (printf "(count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)): ~a \n" (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))
  (check-equal? (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) 
                5 
                "Error for: (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))")

  (printf "(suit-counts '(sa s10 hq ck c4)): ~a \n" (suit-counts '(sa s10 hq ck c4)))
  (check-equal? (suit-counts '(sa s10 hq ck c4)) 
                '(2 0 1 2) 
                "Error for: (suit-counts '(sa s10 hq ck c4))")
  (printf "(suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)): ~a \n" (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)))
  (check-equal? (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) 
                '(2 3 3 5) 
                "Error for: (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))")
  (printf "(suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)): ~a \n" (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))
  (check-equal? (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) 
                '(2 5 1 5) 
                "Error for: (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))")
  (printf "(suit-counts '(s4 s5 s9 c4 h6 h5)): ~a \n" (suit-counts '(s4 s5 s9 c4 h6 h5)))
  (check-equal? (suit-counts '(s4 s5 s9 c4 h6 h5)) '(1 0 2 3) "Error for (suit-counts '(s4 s5 s9 c4 h6 h5))")
  (define suit-count-a (suit-counts '(s4 s5 s9 c4 h6 h5)))
  (printf "(num-clubs (suit-counts '(s4 s5 s9 c4 h6 h5))): ~a \n" (num-clubs (suit-counts '(s4 s5 s9 c4 h6 h5))))
  (check-equal? (num-clubs (suit-counts '(s4 s5 s9 c4 h6 h5))) 1 "Error for (num-clubs (suit-counts '(s4 s5 s9 c4 h6 h5)))")

(printf "(num-diamonds (suit-counts '(s4 s5 s9 c4 h6 h5))): ~a \n" (num-diamonds (suit-counts '(s4 s5 s9 c4 h6 h5))))
(check-equal? (num-diamonds (suit-counts '(s4 s5 s9 c4 h6 h5))) 0 "Error for (num-diamonds (suit-counts '(s4 s5 s9 c4 h6 h5)))")

(printf "(num-hearts (suit-counts '(s4 s5 s9 c4 h6 h5))): ~a \n" (num-hearts (suit-counts '(s4 s5 s9 c4 h6 h5))))
(check-equal? (num-hearts (suit-counts '(s4 s5 s9 c4 h6 h5))) 2 "Error for (num-hearts (suit-counts '(s4 s5 s9 c4 h6 h5)))")

(printf "(num-spades (suit-counts '(s4 s5 s9 c4 h6 h5))): ~a \n" (num-spades (suit-counts '(s4 s5 s9 c4 h6 h5))))
(check-equal? (num-spades (suit-counts '(s4 s5 s9 c4 h6 h5))) 3 "Error for (num-spades (suit-counts '(s4 s5 s9 c4 h6 h5)))")


  ; (printf ": ~a \n" )
  ; (check-equal?  "Error for: ")

) ;; end module+ test 
  ; (printf ": ~a \n"  )
; (check-equal?  "Error for: ")

