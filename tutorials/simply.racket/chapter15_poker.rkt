#lang simply-scheme

; Chapter 15 project: Poker

(require (prefix-in more: "more-simply.rkt")
         (prefix-in ch19: "chapter19.rkt"))

(butfirst '(This is chapter 15 poker))
;; also see
;; https://en.wikipedia.org/wiki/Texas_hold_%27em#Hand_values
;; https://en.wikipedia.org/wiki/List_of_poker_hands
#|
- Royal flush: ten, jack, queen, king, and ace, all of the same suit
- Straight flush: five cards of sequential rank, all of the same suit
- Four of a kind: four cards of the same rank
- Full house: three cards of the same rank, and two of a second rank
- Flush: five cards of the same suit, not sequential rank
- Straight: five cards of sequential rank, not all of the same suit
- Three of a kind: three cards of the same rank, no other matches
- Two pair: two pairs of cards, of two different ranks
- Pair: two cards of the same rank, no other matches
- Nothing: none of the above 
|#

;; look at location from chapter14
;; It should return a number indicating where in the sentence that word can be found.
;; from chapter 12
(define (spell-digit-plural digit)
  (item (+ 1 digit)
	'(zeroes ones twos threes fours fives sixes sevens eights nines tens jacks queens kings aces)))
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

(define (rank-counts card-sentence)
  (every (lambda (x) (count-rank x card-sentence)) 
         '(0 1 2 3 4 5 6 7 8 9 10 j q k a)))
;; a hand with a 6 and 4 kings gives:
;; '(0 0 0 0 0 0 1 0 0 0 0 0 0 4 0) it starts with 0
;; this could be filter, but I think for chapter 15 we are supposed to do it by hand
(define (get-rank-numbers-hlpr rank-sentence number count output)
  (cond [(empty? rank-sentence) output] 
    [(equal? (car rank-sentence ) number) 
         (begin
           (append output count)
           (get-rank-numbers-hlpr (cdr rank-sentence) number (+ 1 count) (output)))]
        [else (get-rank-numbers-hlpr (cdr rank-sentence) number (+ 1 count) output)]))
;; (get-number-from-rank-count '(0 0 0 0 0 0 1 0 0 0 0 0 0 4 0) 4)
(define (get-numbers-from-rank-count rank-sentence number)
  (get-rank-numbers-hlpr rank-sentence number 0 '()))
; '(da d6 d3 c9 h6) -> card-sentence, like in the book
; (check-for-single-pair '(da d6 d3 c9 h6))
; (check-for-single-pair '(da d6 d3 c9 h6))
(define (check-for-single-pair card-sentence)
  (more:display-all "Result of apprearances 2 on sent " card-sentence ": " (appearances 2 (rank-counts card-sentence)))
  (if (equal? 1 (appearances 2 (rank-counts card-sentence)))
    #t
    #f))
; (check-for-two-pairs '(ck hk d7 c7 s5))
(define (check-for-two-pairs card-sentence)
  (if (= 2 (appearances 2 (rank-counts card-sentence)))
    #t
    #f))
;; (check-for-four-of-a-kind '(dk d6 sk ck hk))
(define (check-for-four-of-a-kind card-sentence)
  (if (= 1 (appearances 4 (rank-counts card-sentence)))
    #t
    #f))
; check for a possible straight
(define (check-for-poss-straight card-sentence)
  (if (= 5 (appearances 1 (rank-counts card-sentence)))
    #t
    #f))

(define (check-for-three-of-a-kind card-sentence)
  (if (= 1 (appearances 3 (rank-counts card-sentence)))
    #t
    #f))

(define (check-for-full-house card-sentence)
  (if (and (check-for-three-of-a-kind card-sentence) (check-for-single-pair card-sentence))
   #t
   #f))

; regular: (check-flush '(ck cq c9 c8 c2))
; straight: (check-flush '(c3 c4 c5 c6 c7))
; royal: going by Texas Hold'em page: (check-flush '(h10 hj hq hk ha))
(define (butfirst-before? wd1 wd2)
  (before? (butfirst wd1) (butfirst wd2)))

(define (convert-face-to-num face)
  (cond [(equal? face 'j) 11]
        [(equal? face 'q) 12]
        [(equal? face 'k) 13]
        [(equal? face 'a) 14]
        [else #f]))

;; convert face cards to numbers
(define (change-card-sen-hlpr input output)
  ;(more:display-all "change card helper with input: " input ", output: " output)
  (cond [(empty? input) (reverse output)]
        [(and (not (number? (butfirst (car input)))) (member? (butfirst (car input)) 'jqka)) 
         (change-card-sen-hlpr (cdr input)
                               (cons (word (first (car input)) 
                                           (convert-face-to-num (butfirst (car input)))) 
                                     output))]
        [(and (not (equal? 10 (butfirst (car input)))) (member? (butfirst (car input)) '23456789))
         (change-card-sen-hlpr (cdr input)
                               (cons (word (first (car input)) 0 (butfirst (car input)))
                                     output))]
        [else (change-card-sen-hlpr (cdr input) (cons (car input) output))]))

; (change-card-sentence '(d2 cj h3 s10))
; '(d02 c11 h03 s10)
(define (change-card-sentence card-sentence)
  (change-card-sen-hlpr card-sentence '()))

; check for straight:
; change-card-sentence to convert face cards
; call ch19:sort-19-list with butfirst-before?
; compare number of first to number of last
; or if first is a two and last is an ace, and next-to-last is 5

; (ch19:sort-19-list (change-card-sentence '(ca h4 d7 ck s2)) butfirst-before?) 
; you might be able to use reduce to check for a straight
; use the first value as the baseline
; or look at 14.10
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

  (check-equal? (change-card-sentence '(d2 cj h3 s10)) '(d02 c11 h03 s10))

  (check-equal? (ch19:sort-19-list (change-card-sentence '(ca h4 d7 ck s2)) 
                                   butfirst-before?) 
                '(s02 h04 d07 c13 c14))

  ; (printf ": ~a \n" )
  ; (check-equal?  "Error for: ")

) ;; end module+ test 
  ; (printf ": ~a \n"  )
; (check-equal?  "Error for: ")

