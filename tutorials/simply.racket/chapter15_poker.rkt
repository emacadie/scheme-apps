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

(define (count-rank rank card-list)
  (count (keep (lambda (x) (equal? rank x)) (every butfirst card-list))))

; (rank-counts '(d3 ca hk s10 h4))
; gives: '(0 0 0 1 1 0 0 0 0 0 1 0 0 1 1)
; (rank-counts '(h4 s4 c6 s6 c4))
(define (rank-counts card-sentence)
  (every (lambda (x) (count-rank x card-sentence)) 
         '(0 1 2 3 4 5 6 7 8 9 10 j q k a)))
;; a hand with a 6 and 4 kings gives:
;; '(0 0 0 0 0 0 1 0 0 0 0 0 0 4 0) it starts with 0
;; this could be filter, but I think for chapter 15 we are supposed to do it by hand
; (get-numbers-from-rank-count (rank-counts '(ck hk d7 c7 s5)) 2)
; (get-numbers-from-rank-count (rank-counts '(h4 s4 c6 s6 c4)) 2)
(define (get-rank-numbers-hlpr rank-sentence number count output)
  (cond [(empty? rank-sentence) output] 
    [(equal? (car rank-sentence ) number) 
         (begin
           (get-rank-numbers-hlpr (cdr rank-sentence) 
                                  number 
                                  (+ 1 count) 
                                  (append output (list count))))]
        [else (get-rank-numbers-hlpr (cdr rank-sentence) 
                                     number 
                                     (+ 1 count) 
                                     output)]))

;; (get-numbers-from-rank-count '(0 0 0 0 0 0 1 0 0 0 0 0 0 4 0) 4)
; (get-numbers-from-rank-count '(0 0 0 0 0 0 1 0 0 0 0 0 0 4 0) 4)
; gives 13
; rank sentence comes from rank-counts
(define (get-numbers-from-rank-count rank-sentence number)
  (get-rank-numbers-hlpr rank-sentence number 0 '()))
; '(da d6 d3 c9 h6) -> card-sentence, like in the book
; (check-for-single-pair '(da d6 d3 c9 h6))
; (check-for-single-pair '(da d6 d3 c9 h6))
(define (check-for-single-pair card-sentence)
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
  (if (and (check-for-three-of-a-kind card-sentence) 
           (check-for-single-pair card-sentence))
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
  (cond [(empty? input) (reverse output)]
        [(and (not (number? (butfirst (car input)))) 
              (member? (butfirst (car input)) 'jqka)) 
         (change-card-sen-hlpr (cdr input)
                               (cons (word (first (car input)) 
                                           (convert-face-to-num (butfirst (car input)))) 
                                     output))]
        [(and (not (equal? 10 (butfirst (car input)))) 
              (member? (butfirst (car input)) '23456789))
         (change-card-sen-hlpr (cdr input)
                               (cons (word (first (car input)) 
                                           0 
                                           (butfirst (car input)))
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
(define (first-last-diff sorted-sent)
  (- (butfirst (list-ref sorted-sent 4))
     (butfirst (list-ref sorted-sent 0))))

(define (first-is-two sorted-sent)
  (cond [(equal? "02" (butfirst (first sorted-sent))) #t]
        [else #f]))
(define (first-is-ten sorted-sent)
  (cond [(equal? "10" (butfirst (first sorted-sent))) #t]
        [else #f]))
(define (last-is-ace sorted-sent)
  (cond [(equal? "14" (butfirst (last sorted-sent))) #t]
        [else #f]))
(define (next-to-last-is-five sorted-sent)
  (cond [(equal? "05" (butfirst (last (butlast sorted-sent)))) #t]
        [else #f]))

(define (check-for-straight card-sentence)
  (let ([sorted-sent (ch19:sort-19-list 
                      (change-card-sentence card-sentence)
                      butfirst-before?)])
    (cond [(equal? 4 (first-last-diff sorted-sent)) #t]
          [(and (first-is-two sorted-sent)
                (last-is-ace sorted-sent)
                (next-to-last-is-five sorted-sent)) #t]
          [else #f])))

(define (check-royal card-sentence)
  (let ([sorted-sent (ch19:sort-19-list 
                      (change-card-sentence card-sentence)
                      butfirst-before?)])
    (cond [(and (first-is-ten sorted-sent)
                (last-is-ace sorted-sent)) #t]
          [else #f])))

; (ch19:sort-19-list (change-card-sentence '(ca h4 d7 ck s2)) butfirst-before?) 
; you might be able to use reduce to check for a straight
; use the first value as the baseline
; or look at 14.10
(define (check-flush card-sentence)
  (check-flush-work (suit-counts card-sentence)))

(define (check-flush-work list-suit-counts)
  (cond [(equal? 0 (appearances 5 list-suit-counts))      'none]
        [(equal? 5 (first list-suit-counts))              'clubs]
        [(equal? 5 (more:simply-second list-suit-counts)) 'diamonds]
        [(equal? 5 (last (butlast list-suit-counts)))     'hearts]
        [(equal? 5 (last list-suit-counts))               'spades]))

(define (get-high-num card-sentence)
  (car (reverse (get-numbers-from-rank-count (rank-counts card-sentence) 1))))

(define (make-two-pair-sentence rank-sentence)
  (sentence 'pair 'of 
            (word (spell-digit-plural (car (get-numbers-from-rank-count rank-sentence 2)))) 
            'and 'pair 'of 
            (word (spell-digit-plural (list-ref (get-numbers-from-rank-count rank-sentence 2) 1)))))

(define (make-full-house-sentence rank-sentence)
  (sentence 'full 'house 
            (word (spell-digit-plural (car (get-numbers-from-rank-count rank-sentence 3)))) 
            'over 
            (word (spell-digit-plural (car (get-numbers-from-rank-count rank-sentence 2))))))

(define (make-kind-sentence rank-sentence label num)
  (sentence label 
            (word (spell-digit-plural (car (get-numbers-from-rank-count rank-sentence num))))))

;; here is the main event
(define (poker-value card-sentence)
  (let ([rank-sentence (rank-counts card-sentence)])
      (cond ; royal flush
        [(and (check-royal card-sentence) 
                  (not (equal? 'none (check-flush card-sentence))))
             (sentence 'royal 'flush '- (check-flush card-sentence))]
        ; straight flush
        [(and (check-for-straight card-sentence)
                  (not (equal? 'none (check-flush card-sentence))))
             (sentence 'straight 'flush (spell-digit-plural (get-high-num card-sentence)) 'high)]
        ; four of a kind
        [(check-for-four-of-a-kind card-sentence)
         (make-kind-sentence rank-sentence 'four-of-a-kind 4)]
        ; full house
        [(check-for-full-house card-sentence) (make-full-house-sentence rank-sentence)]
        ; flush
        [(not (equal? 'none (check-flush card-sentence)))
             (sentence 'flush (spell-digit-plural (get-high-num card-sentence)) 'high (check-flush card-sentence))]
        ; straight
        [(check-for-straight card-sentence)
         (sentence 'straight (spell-digit-plural (get-high-num card-sentence)) 'high)]
        ; three of a kind
        [(check-for-three-of-a-kind card-sentence)
         (make-kind-sentence rank-sentence 'three-of-a-kind 3)]
        ; two pair
        [(check-for-two-pairs card-sentence) (make-two-pair-sentence rank-sentence)]
        ; pair
        [(check-for-single-pair card-sentence)
         (make-kind-sentence rank-sentence 'pair-of 2)]
        [else '(no dice)])))


(module+ test
  (require rackunit)
  (check-true #t)

  (printf "checking count-suit \n")
  (check-equal? (count-suit 's '(sa s10 hq ck c4)) 
                2 
                "Error for: (count-suit 's '(sa s10 hq ck c4))")
  (check-equal? (count-suit 'c '(c3 d8 dj c10 d5)) 
                2 
                "Error for: (count-suit 'c '(c3 d8 dj c10 d5))")
  (check-equal? (count-suit 'd '(c3 d8 dj c10 d5)) 
                3 
                "Error for: (count-suit 'd '(c3 d8 dj c10 d5))")
   
  ;; hands that we can use
  (printf "checking suit-counts \n")
  (check-equal? (suit-counts '(c3 d8 dj c10 d5)) '(2 3 0 0) "Error for (suit-counts '(c3 d8 dj c10 d5))")
  (check-equal? (suit-counts '(s5 cj ca h2 h7))  '(2 0 2 1) "Error for (suit-counts '(s5 cj ca h2 h7))")
  (check-equal? (suit-counts '(h8 d4 d10 c10 ha))  '(1 2 2 0) "Error for (suit-counts '(h8 d4 d10 c10 ha))")

  (check-equal? (change-card-sentence '(d2 cj h3 s10)) '(d02 c11 h03 s10))

  (check-equal? (ch19:sort-19-list (change-card-sentence '(ca h4 d7 ck s2)) 
                                   butfirst-before?) 
                '(s02 h04 d07 c13 c14))

  (check-equal? '(13) 
                (get-numbers-from-rank-count '(0 0 0 0 0 0 1 0 0 0 0 0 0 4 0)  
                                             4))
  (check-equal? '(6) 
                (get-numbers-from-rank-count (rank-counts '(h4 s4 c6 s6 c4)) 
                                             2))
  (check-equal? '(7 13) 
                (get-numbers-from-rank-count (rank-counts '(ck hk d7 c7 s5)) 
                                             2))

  (check-equal? #t (check-for-straight '(d3 c7 h5 s6 d3)))
  (check-equal? #f (check-for-straight '(d3 c4 h5 s6 d8)))
  (check-equal? #t (check-for-straight '(ca dj s10 hk cq)))
  (check-equal? #t (check-for-straight '(h4 ca d3 s5 c2)))
  (check-equal? #f (check-for-straight '(h2 c3 dq sk ca)))

  (check-equal? #t (check-royal '(c10 da sk hq cj)))
  (check-equal? #f (check-royal '(c10 da sk h9 cj)))

  (check-equal? '(pair-of sixes) (poker-value '(da d6 d3 c9 h6)))
  (check-equal? '(pair of threes and pair of jacks) 
                (poker-value '(hj sj c3 s3 h2)))
  (check-equal? '(three-of-a-kind queens) (poker-value '(cq h9 sq hq s2)))
  (check-equal? '(straight tens high) (poker-value '(d10 s9 h8 d7 c6)))
  (check-equal? '(flush kings high clubs) (poker-value '(c9 c2 ck c8 cq)))
  (check-equal? '(flush jacks high diamonds) (poker-value '(d8 dj d3 d9 d4)))
  (check-equal? '(full house fours over sixes) (poker-value '(h4 s4 c6 s6 c4)))
  (check-equal? '(full house sixes over kings) (poker-value '(hk h6 d6 sk c6)))
  (check-equal? '(four-of-a-kind kings) (poker-value '(dk ck d5 sk hk)))
  (check-equal? '(straight flush sevens high) (poker-value '(d3 d4 d5 d6 d7)))
  (check-equal? '(straight flush sevens high) (poker-value '(d6 d7 d5 d3 d4)))
  (check-equal? '(royal flush - hearts) (poker-value '(hq h10 ha hk hj)))


) ;; end module+ test 

