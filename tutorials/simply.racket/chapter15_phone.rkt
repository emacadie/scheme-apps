#lang simply-scheme

; Chapter 15: Advanced Recursion, Phone problem

(require "more-simply.rkt")

(butfirst '(This is chapter 15 advanced recursion phone problem))

;; Chapter 15 Advanced Recursion

;; Again, I think tail-recursion results in larger code

;; From the text:
;; "Sometimes you write a recursive procedure with a correct recursive case and a reasonable base case, 
;; but the program still doesn't work. 
;; The trouble may be that the base case doesn't quite catch all of the ways in which the problem can get smaller. 
;; A second base case may be needed."
;; I don't know what this says about me, but I have already come across that. 
;; Perhaps because I am trying to make everything tail-recursive.

;; more words of wisdom from the text:
;; You may be thinking that you never would have thought of that yourself. 
;; But we're just following the method: 
;; Look at the smaller case and see how it fits into the original problem.

;; 15.3  Write a procedure substrings that takes a word as its argument. 
;; It should return a sentence containing all of the substrings of the argument. 
;; A substring is a subset whose letters come consecutively in the original word. 
;; For example, the word bat is a subset, but not a substring, of brat.
;; One drawback: If a letter appears multiple times in word, it will be in list multiple times
(define (substrings-r the-word  outp)
  (cond [(empty? the-word) outp]
        [(empty? outp) (substrings-r (butfirst the-word) (sentence (word (first the-word))))]
        [else (substrings-r (butfirst the-word) (sentence outp (word (last outp) (first the-word))))]))

(define (sub-whole-word-r the-word outp)
  (cond [(empty? the-word) outp]
        [(empty? outp) 
         ;; Why does the else skip the second letter?
         (sub-whole-word-r (butfirst the-word) (sentence (substrings-r the-word '()) (substrings-r (butfirst the-word) '())))] ; 
        [else
         ; (display-all "in else for sub-whole-wordr, outp: " outp)
         (sub-whole-word-r (butfirst the-word) (sentence outp (substrings-r (butfirst the-word) '())))]))

(define (substring-main the-word)
  (sub-whole-word-r the-word '()))

;; Here is what the other guys did:
(define (substring-helper-o wd)
  (if (empty? wd)
      '()
      (se wd (substring-helper-o (bl wd)))))

(define (substring-o wd)
  (if (empty? wd)
      (se "")
      (se (substring-helper-o wd) (substring-o (bf wd)))))

;; Can I do it tail-recursive? Perhaps then I will not have duplicates
;; I just tried it, they also have dupes

;; Here they are anyway
(define (substring-helper-o-r wd outp)
  (if (empty? wd)
      outp
      (substring-helper-o-r (bl wd) (sentence wd outp))))

(define (substring-o-r wd outp)
  (if (empty? wd) 
      outp
      (substring-o-r (butfirst wd) (sentence outp (substring-helper-o-r wd '())))))

;; I do tend to prefer "cond" over "if". Is that wrong?

;; 15.5 Suppose you have a phone number, such as 223-5766, and you'd like to figure out a clever way to spell it in letters for your friends to remember. 
;; Each digit corresponds to three possible letters. 
;; For example, the digit 2 corresponds to the letters A, B, and C. 
;; Write a procedure that takes a number as argument and returns a sentence of all the possible spellings:
;; > (phone-spell 2235766)
;; (AADJPMM AADJPMN ...CCFLSOO)
;; (We're not showing you all 2187 words in this sentence.) 
;; You may assume there are no zeros or ones in the number, since those don't have letters.
;; Hint: This problem has a lot in common with the subsets example. 
(define big-table '(1 'abc 'def 'ghi 'jkl 'mno 'pqrs 'tuv 'wxyz))
(define (table-item num)
  (cond [(equal? num 0) 0]
        [(member? num '123456789) (item num big-table) ]
        [else 0]))
(define 2-table 'abc)
(define 3-table 'def)
(define 4-table 'ghi)
(define 5-table 'jkl)
(define 6-table 'mno)
(define 7-table 'pqrs)
(define 8-table 'tuv)
(define 9-table 'wxyz)

(define (get-letter-max num)
  (cond [(member? num '79) 4]
        [(member? num '01234568) 3]
        [else 0]))

(define (get-letter root-num place-in-table)
  (cond [(not (number? place-in-table)) root-num]
        [(not (positive? place-in-table)) root-num]
        [(> place-in-table (get-letter-max root-num)) root-num ]
        [(equal? root-num 2) (item place-in-table 'abc)]
        [(equal? root-num 3) (item place-in-table 'def)]
        [(equal? root-num 4) (item place-in-table 'ghi)]
        [(equal? root-num 5) (item place-in-table 'jkl)]
        [(equal? root-num 6) (item place-in-table 'mno)]
        [(equal? root-num 7) (item place-in-table 'pqrs)]
        [(equal? root-num 8) (item place-in-table 'tuv)]
        [(equal? root-num 9) (item place-in-table 'wxyz)]
        [else root-num])) ; line 182
;; gotta be recursive
; (define (for-one-number the-num place '())
;   (cond []))
; (get-first-list 2 1 '()) ; always call with num-place of 1
(define (get-first-list num num-place outp)
  (cond [(member? num '01) (list num)] 
    [(> num-place (get-letter-max num)) outp]
        [else (get-first-list num 
                              (+ 1 num-place) 
                              (sentence outp (get-letter num num-place)))]))
; (get-letter-list 2 1 'wt '())  returns '(wta wtb wtc)
; (get-letter-list 4 1 'a '()) returns '(ag ah ai)
; (sentence (get-letter-list 2 1 'fjb '()) (get-letter-list 2 1 'fjc '()))
; gives '(fjba fjbb fjbc fjca fjcb fjcc)
; call with num-place of 1
(define (get-letter-list num num-place input outp)
  (cond [(> num-place (get-letter-max num)) outp]
        [else (get-letter-list num 
                              (+ 1 num-place) 
                              input
                              (sentence outp 
                                        (word input 
                                              (get-letter num num-place))))]))

(define (spell-phone-num pnum num input outp)
  (cond [(empty? pnum) outp] 
        [(empty? outp) (spell-phone-num (butfirst pnum) 
                                        0 
                                        '()
                                        (get-first-list (first pnum) 1 '()))]
        [else (spell-phone-num (butfirst pnum) 
                               (first pnum) 
                               '() 
                               (sentence outp (get-letter-list (first pnum) 
                                                               1 
                                                               (first outp) 
                                                               '() ))
)]
                       
)
)

(define (get-next-list num num-place input outp)
  (cond [(empty? input) outp]
        [else outp]
)
)
(define (phone-spell-r the-num outp)
  (display-all "calling phone-spell-r with the-num: " the-num ", outp: " outp)
  (cond [(empty? the-num) outp]
        [else (phone-spell-r (butfirst the-num) 
                             (sentence outp 
                                       (get-letter (first the-num) 
                                                   1)))]))

(define (get-first-spelling ph-num outp)
  (display-all "calling get-first-spelling with ph-num: " ph-num ", outp: " outp)
  (cond [(empty? ph-num) outp]
        ;; "word" croaks if you send it an empty list
        [(empty? outp) (get-first-spelling (butfirst ph-num)
                                  (word (get-letter (first ph-num) 
                                                    1)))]
        [else (get-first-spelling (butfirst ph-num)
                                  (word outp 
                                        (get-letter (first ph-num) 
                                                    1)))]))

;; (try-something 2254876 aajgtpm  aajgtpm 3 6 '())
;; (try-something 2254876 'aajgtpm  'aajgtpm (get-letter-max 6) 6 '())
;; returns (aajgtpm aajgtpn aajgtpo)
(define (try-something orig-nums first-spelling first2 counter digit outp)
  (display-all "try-something, orig-nums: " orig-nums ", first-spelling: " first-spelling ", first2: " first2 ", counter: " counter ", digit: " digit ", outp: " outp)
  (cond [(equal? counter 0) outp]
        [else (try-something orig-nums 
                             first-spelling 
                             first2 
                             (- counter 1) 
                             digit 
                             (sentence (word first2 
                                             (get-letter digit counter)) 
                                       outp))]))

; (define (work-on-last-two orig-num))
(define (phone-spell the-num)
  (phone-spell-r the-num '())
)


(module+ test
  (require rackunit)
  (check-true #t)
  ; (printf "(who '(sells out)): ~a \n" (who '(sells out)))
  ; (check-equal? (who '(sells out)) '(pete sells out roger sells out john sells out keith sells out) "Error for (who '(sells out))")

  (printf "(substring-o-r 'hello '()): ~a \n" (substring-o-r 'hello '()))
  (check-equal? (substring-o-r 'hello '()) 
                '(h he hel hell hello e el ell ello l ll llo l lo o)
                "Error for: (substring-o-r 'hello '())")
  (printf "(sub-whole-word-r 'hello '()): ~a \n" (sub-whole-word-r 'hello '()))
  (check-equal? (sub-whole-word-r 'hello '()) 
                '(h he hel hell hello e el ell ello l ll llo l lo o) 
                "Error for: (sub-whole-word-r 'hello '())")

  ; (printf ": ~a \n" )
  ; (check-equal?  "Error for: ")

) ;; end module+ test 
  ; (printf ": ~a \n"  )
  ; (check-equal?  "Error for: ")
