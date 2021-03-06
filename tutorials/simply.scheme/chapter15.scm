;; Chapter 15 Advanced Recursion

;; Again, I think tail-recursion results in larger code

;; From the text:
;; "Sometimes you write a recursive procedure with a correct recursive case and a reasonable base case, 
;; but the program still doesn't work. 
;; The trouble may be that the base case doesn't quite catch all of the ways in which the problem can get smaller. 
;; A second base case may be needed."
;; I don't know what this says about me, but I have already come across that. 
;; Perhaps because I am trying to make everything tail-recursive.

;; 15.1  Write a procedure to-binary:
; > (to-binary 9)
;; 1001
;; > (to-binary 23)
;; 10111

(define (from-binary-r bin-num counter outp)
  (display-all "calling from-binary-r with bin-num: " bin-num  ", counter: " counter ", outp: " outp)
  (cond ((empty? bin-num) outp)
        ((equal? (last bin-num) 0) (from-binary-r (butlast bin-num) (+ counter 1) outp))
        ;; else it's equal to 1, right? 
        (else
         ; (display-all "In the else: (last bin-num) is " (last bin-num) ", (expt 2 (last bin-num) is " (expt 2 (counter)))
         (from-binary-r (butlast bin-num) (+ counter 1) (+ outp (expt 2 counter))))))

(define (from-binary bin-num)
  (from-binary-r (trim-leading-zeros bin-num) 0 0))

(define (closest-power-of-2 the-num outp)
  ; (display-all "closest-power-of-2 with the-num: " the-num ", outp: " outp ", (expt 2 outp) is: " (expt 2 outp))
  (cond ((> (expt 2 outp) the-num) (- outp 1))
        ((equal? (expt 2 outp) the-num) outp)
        (else (closest-power-of-2 the-num (+ 1 outp)))))

; This actually gets a list of where the ones go
; come up with a better name later
(define (get-all-powers-of-2 the-num outp)
  ; (display-all "get-all-powers-of-2 with the-num: " the-num ", outp: " outp)
  (cond ((equal? the-num 0) 
         ; (display-all "in first cond of get-all-powers-of-2")
         outp)
        ((equal? the-num 2) (sentence 1 outp))
        (else
         ; (display-all "in else for get-all-powers-of-2")
         (get-all-powers-of-2 (- the-num (expt 2 (closest-power-of-2 the-num 0))) 
                              (sentence (+ (closest-power-of-2 the-num 0) 1) outp)))))

(define (to-binary-r power-list counter outp)
  ; (display-all "in to-binary-r with power-list: " power-list ", counter: " counter ", outp: " outp)
  (cond ((empty? power-list) outp)
        ((equal? (first power-list) counter) (to-binary-r (butfirst power-list) (+ 1 counter) (sentence 1 outp)))
        (else (to-binary-r power-list (+ 1 counter) (sentence 0 outp)))))
;; call this
(define (to-binary dec-num)
  (to-binary-r (get-all-powers-of-2 (trim-leading-zeros dec-num) '()) 
               1
               '()))
;; I think this works
;; Look at other solutions

;; 15.2  A "palindrome" is a sentence that reads the same backward as forward. Write a predicate palindrome? that takes a sentence as argument and decides whether it is a palindrome. For example:
;; > (palindrome? '(flee to me remote elf))
;; #T
;; > (palindrome? '(flee to me remote control))
;; #F
;; Do not reverse any words or sentences in your solution. 
(define (remove-spaces the-sent)
  (accumulate word the-sent))

(define (palindrome-r the-word outp)
  (display-all "calling palindrome-r with the-word: " the-word ", outp: " outp)
  (cond ((empty? the-word) #t)
        ((equal? (count the-word) 1) #t)
        ((not (equal? (first the-word) (last the-word))) #f)
        (else (palindrome-r (word (butfirst (butlast the-word))) #t))))

(define (palindrome? the-sent)
  (palindrome-r (remove-spaces the-sent) #f))

;; 15.3  Write a procedure substrings that takes a word as its argument. 
;; It should return a sentence containing all of the substrings of the argument. 
;; A substring is a subset whose letters come consecutively in the original word. 
;; For example, the word bat is a subset, but not a substring, of brat.
;; One drawback: If a letter appears multiple times in word, it will be in list multiple times
(define (substrings-r the-word  outp)
  ; (display-all "calling subsrings-r with the-word: " the-word ", outp: " outp)
  (cond ((empty? the-word) outp)
        ((empty? outp) (substrings-r (butfirst the-word) (sentence (word (first the-word)))))
        (else (substrings-r (butfirst the-word) (sentence outp (word (last outp) (first the-word)))))))

(define (sub-whole-word-r the-word outp)
  ; (display-all "calling sub-whole-word-r, the-word: " the-word ", outp: " outp)
  (cond ((empty? the-word) outp)
        ((empty? outp) 
         ; (display-all "in sub-whole-word-r, outp empty: " outp)
         ;; Why does the else skip the second letter?
         (sub-whole-word-r (butfirst the-word) (sentence (substrings-r the-word '()) (substrings-r (butfirst the-word) '()) ))) ; 
        (else
         ; (display-all "in else for sub-whole-wordr, outp: " outp)
         (sub-whole-word-r (butfirst the-word) (sentence outp (substrings-r (butfirst the-word) '()))))))

(define (subsets the-word)
  (sub-whole-word-r the-word '()))

;; Here is what the other guy's did:
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

;; 15.4 Write a predicate procedure substring? that takes two words as arguments and returns #t if and only if the first word is a substring of the second. 
;; (See Exercise 15.3 for the definition of a substring.)
;; Be careful about cases in which you encounter a "false start," like this:
;; > (substring? 'ssip 'mississippi)
; #T
;; and also about subsets that don't appear as consecutive letters in the second word:
;; > (substring? 'misip 'mississippi)

(define (substring? sub the-string)
  (member? sub (sub-whole-word-r the-string '())))

;; 15.5 Suppose you have a phone number, such as 223-5766, and you'd like to figure out a clever way to spell it in letters for your friends to remember. 
;; Each digit corresponds to three possible letters. 
;; For example, the digit 2 corresponds to the letters A, B, and C. 
;; Write a procedure that takes a number as argument and returns a sentence of all the possible spellings:
;; > (phone-spell 2235766)
;; (AADJPMM AADJPMN ...CCFLSOO)
;; (We're not showing you all 2187 words in this sentence.) You may assume there are no zeros or ones in the number, since those don't have letters.
;; Hint: This problem has a lot in common with the subsets example. 
(define 2-table 'abc)
(define 3-table 'def)
(define 4-table 'ghi)
(define 5-table 'jkl)
(define 6-table 'mno)
(define 7-table 'pqrs)
(define 8-table 'tuv)
(define 9-table 'wxyz)

(define (get-letter root-num place-num)
  (cond ((equal? root-num 2) (item place-num 2-table))
        ((equal? root-num 3) (item place-num 3-table))
        ((equal? root-num 4) (item place-num 4-table))
        ((equal? root-num 5) (item place-num 5-table))
        ((equal? root-num 6) (item place-num 6-table))
        ((equal? root-num 7) (item place-num 7-table))
        ((equal? root-num 8) (item place-num 8-table))
        ((equal? root-num 9) (item place-num 9-table))
        (else root-num)))

(define (get-letter-max num)
  (if (member? num '79)
      4
      3))

(define (phone-spell-r the-num outp)
  (display-all "calling phone-spell-r with the-num: " the-num ", outp: " outp)
  (cond ((empty? the-num) outp)
        (else (phone-spell-r (butfirst the-num) 
                             (sentence outp 
                                       (get-letter (first the-num) 
                                                   1))))))

(define (get-first-spelling ph-num outp)
  (display-all "calling get-first-spelling with ph-num: " ph-num ", outp: " outp)
  (cond ((empty? ph-num) outp)
        ;; "word" croaks if you send it an empty list
        ((empty? outp) (get-first-spelling (butfirst ph-num)
                                  (word (get-letter (first ph-num) 
                                                    1))))
        (else (get-first-spelling (butfirst ph-num)
                                  (word outp 
                                        (get-letter (first ph-num) 
                                                    1))))))

;; (try-something 2254876 aajgtpm  aajgtpm 3 6 '())
;; returns (aajgtpm aajgtpn aajgtpo)
(define (try-something orig-nums first-spelling first2 counter digit outp)
  (display-all "try-something, orig-nums: " orig-nums ", first-spelling: " first-spelling ", first2: " first2 ", counter: " counter ", digit: " digit ", outp: " outp)
  (cond ((equal? counter 0) outp)
        (else (try-something orig-nums first-spelling 
                             first2 
                             (- counter 1) 
                             digit 
                             (sentence (word (butlast first2) 
                                             (get-letter digit counter)) 
                                       outp)))))

(define (work-on-last-two orig-num))
(define (phone-spell the-num)
  (phone-spell-r the-num '())
)


;; come back to this later
;; 15.6  Let's say a gladiator kills a roach. 
;; If we want to talk about the roach, we say "the roach the gladiator killed." 
;; But if we want to talk about the gladiator, we say "the gladiator that killed the roach."

;; People are pretty good at understanding even rather long sentences as long as they're straightforward: 
;; "This is the farmer who kept the cock that waked the priest that married the man that kissed the maiden that milked the cow that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built." 
;; But even a short nested sentence is confusing: 
;; "This is the rat the cat the dog worried killed." 
;; Which rat was that?
;; Write a procedure unscramble that takes a nested sentence as argument and returns a straightforward sentence about the same cast of characters:
;; > (unscramble '(this is the roach the gladiator killed))
;; (THIS IS THE GLADIATOR THAT KILLED THE ROACH)
;;> (unscramble '(this is the rat the cat the dog the boy the girl saw owned chased bit))
;; (THIS IS THE GIRL THAT SAW THE BOY THAT OWNED THE DOG THAT CHASED THE CAT THAT BIT THE RAT)
;; You may assume that the argument has exactly the structure of these examples, 
;; with no special cases like "that lay in the house" or "that Jack built."

;; only works for those weird types of sentences
;; this is the noun3 the noun2 the noun1 verb1 verb2
(define (unscramble-r the-sent outp)
  (display-all "unscramble-r with the-sent: " the-sent ", outp: " outp)
  (cond ((empty? the-sent) outp)
        ((equal? (count the-sent) 2) (unscramble-r '() 
                                                   (sentence the-sent outp)))
        (else (unscramble-r (sentence (butlast (butfirst (butfirst the-sent)))) 
                            (sentence 'that 
                                      (last the-sent) 
                                      (first the-sent) 
                                      (first (butfirst the-sent)) 
                                      outp)))))

(define (unscramble the-sent)
  (sentence (first the-sent) 
            (first (butfirst the-sent)) 
            (unscramble-r (sentence (butfirst (butfirst the-sent))) 
                          '())))

