#lang simply-scheme

; Chapter 09: Bridge

(require "more-simply.rkt")
; (require "simply-constants.rkt")
(butfirst '(This is chapter 9 bridge))

;; Write a procedure card-val that takes a single card as its argument and returns the value of that card.
;;  Each ace in the hand is worth four points, each king is worth three points, each queen two points, and each jack one. 
;; The other cards, twos through tens, have no point value.
(define (card-val card)
  (cond [(equal? 'a (last card)) 4] 
        [(equal? 'k (last card)) 3]
        [(equal? 'q (last card)) 2]
        [(equal? 'j (last card)) 1]
        [else 0]))

;; Write a procedure high-card-points that takes a hand as its argument and returns the total number of points from high cards in the hand. 
;; (This procedure does not count distribution points.)
(define (high-card-points list-of-cards)
  (accumulate + (every card-val list-of-cards)))

; Write a procedure count-suit that takes a suit and a hand as arguments and returns the number of cards in the hand with the given suit.
(define (count-suit suit card-list)
  (count (keep (lambda (x) (equal? suit x)) (every first card-list))))

;; Write a procedure suit-counts that takes a hand as its argument and returns a sentence containing the number of spades, 
;; the number of hearts, the number of clubs, and the number of diamonds in the hand.
(define (suit-counts suit-sentence)
  (every (lambda (x) (count-suit x suit-sentence)) '(s h c d)))

; Write suit-dist-points that takes a number as its argument, 
; interpreting it as the number of cards in a suit. 
; The procedure should return the number of distribution points your hand gets for having that number of cards in a particular suit.
; from text: A bridge hand might also have some "distribution" points, which are points having to do with the distribution of the thirteen cards among the four suits. 
; If your hand has only two cards of a particular suit, then it is worth an extra point. 
; If it has a "singleton," only one card of a particular suit, that's worth two extra points. 
; A "void," no cards in a particular suit, is worth three points.

(define (suit-dist-points num-suit)
  (cond [(equal? 0 num-suit) 3]
        [(equal? 1 num-suit) 2]
        [(equal? 2 num-suit) 1]
        [else 0]))
;; probably a way to do this with maps, but the book doesn't cover maps

;; Write hand-dist-points, which takes a hand as its argument and returns the number of distribution points the hand is worth.
;; call suit-counts, then suit-dist-points, with accumulate and +
(define (hand-dist-points the-hand)
  (accumulate + (every suit-dist-points (suit-counts the-hand))))

;; Write a procedure bridge-val that takes a hand as its argument and returns the total number of points that the hand is worth.
(define (bridge-val the-hand)
(+ (high-card-points the-hand) (hand-dist-points the-hand)))

(module+ test
  (require rackunit)
  (check-true #t)
  ; (define (ends-vowel? wd) (vowel? (last wd)))
  ; (printf "(who '(sells out)): ~a \n" (who '(sells out)))
  ; (check-equal? (who '(sells out)) '(pete sells out roger sells out john sells out keith sells out) "Error for (who '(sells out))")
  (printf "(card-val 'cq): ~a \n" (card-val 'cq))
  (check-equal? (card-val 'cq) 2 "Error for (card-val 'cq)")
  (printf "(card-val 's7): ~a \n" (card-val 's7))
  (check-equal? (card-val 's7) 0 "Error for (card-val 's7)")
  (printf "(card-val 'ha): ~a \n" (card-val 'ha))
  (check-equal? (card-val 'ha) 4 "Error for (card-val 'ha)")
  
  (printf "(high-card-points '(sa s10 hq ck c4)): ~a \n" (high-card-points '(sa s10 hq ck c4)))
  (check-equal? (high-card-points '(sa s10 hq ck c4)) 9 "Error for: (high-card-points '(sa s10 hq ck c4))")
  (printf "(high-card-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)): ~a \n" (high-card-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)))
  (check-equal? (high-card-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) 13 "Error for: (high-card-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))")
  (printf "(count-suit 's '(sa s10 hq ck c4)): ~a \n" (count-suit 's '(sa s10 hq ck c4)))
  (check-equal? (count-suit 's '(sa s10 hq ck c4)) 2 "Error for: (count-suit 's '(sa s10 hq ck c4))")
  (printf "(count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)): ~a \n" (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) )
  (check-equal? (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) 2 "Error for: (count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))")
  (printf "(count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)): ~a \n" (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))
  (check-equal? (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) 5 "Error for: (count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))")
  (printf "(suit-counts '(sa s10 hq ck c4)): ~a \n" (suit-counts '(sa s10 hq ck c4)))
  (check-equal? (suit-counts '(sa s10 hq ck c4)) '(2 1 2 0) "Error for: (suit-counts '(sa s10 hq ck c4))")
  (printf "(suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)): ~a \n" (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)))
  (check-equal? (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) '(5 3 2 3) "Error for: (suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))")
  (printf "(suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)): ~a \n" (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))
  (check-equal? (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) '(5 1 2 5) "Error for: (suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))")
  (printf "(suit-dist-points 2): ~a \n" (suit-dist-points 2))
  (check-equal? (suit-dist-points 2) 1 "Error for: (suit-dist-points 2)")
  (printf "(suit-dist-points 7) : ~a \n" (suit-dist-points 7))
  (check-equal? (suit-dist-points 7) 0 "Error for: (suit-dist-points 7)")
  (printf "(suit-dist-points 0): ~a \n" (suit-dist-points 0))
  (check-equal? (suit-dist-points 0) 3 "Error for: (suit-dist-points 0)")
  (printf "(hand-dist-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)): ~a \n" (hand-dist-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)))
  (check-equal? (hand-dist-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))  1 "Error for: (hand-dist-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))")
  (printf "(hand-dist-points '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)): ~a \n" (hand-dist-points '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))
  (check-equal? (hand-dist-points '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) 3 "Error for: (hand-dist-points '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))")
  (printf "(bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)): ~a \n"  (bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)))
  (check-equal? (bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3)) 14 "Error for: (bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))")
  (printf "(bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)): ~a \n" (bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)))
  (check-equal? (bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2)) 8 "Error for: (bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))")
) ; line 100
  ; (printf " : ~a \n"  )
  ; (check-equal?  "Error for: ")

