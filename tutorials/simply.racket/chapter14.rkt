#lang simply-scheme

; Chapter 14: Common Patterns in Recursion

(require (prefix-in more: "more-simply.rkt"))
(butfirst '(This is chapter 14 recursion))

;; 14.1  
;; (It's okay if your solution removes the other MORNING instead, as long as it removes only one of them.) 
;; This is sort of like "keep." The result has one less, so it's not "every", and there is more than one, so it's not "accumulate".
(define (remove-once-r bad-word sent outp)
  (cond [(empty? sent) outp]
        [(equal? bad-word (first sent)) (sentence outp (butfirst sent))]
        [else (remove-once-r bad-word 
                             (butfirst sent) 
                             (sentence outp (first sent)))]))

;;  14.2  
(define (up-r the-word outp)
  (cond [(= (count the-word) 0) outp]
        [(= (count outp) 0) 
         (up-r (butfirst the-word) 
               (sentence (first the-word)))]
        [else 
         (up-r (butfirst the-word) 
               (sentence outp 
                         (word (last outp) 
                               (first the-word))))]))
;; This is kind of like every. 

; 14.3  
; (It's okay if your procedure returns (DI OB LA DA) instead, as long as it removes all but one instance of each duplicated word.)
;; This is like "keep".
(define (remove-dup-r sent outp)
  (cond [(empty? sent) outp]
        [(> (appearances (last sent) sent) 1) (remove-dup-r (butlast sent) 
                                                            outp)]        
        [else (remove-dup-r (butlast sent) 
                            (sentence (last sent) outp))]))

;;  14.4  
;; This is like "keep" again
;; This needs a helper
(define (odds the-sent)
  (odds-r the-sent '() 1))

(define (odds-r sent outp counter)
  (cond [(empty? sent) outp]
        [(odd? counter) (odds-r (butfirst sent) 
                                (sentence outp (first sent)) 
                                (- counter 1))]
        [else (odds-r (butfirst sent) 
                      outp 
                      (- counter 1))]))

;;  14.5  [8.7] Write a procedure letter-count that takes a sentence as its argument and returns the total number of letters in the sentence:
;; This one is accumulate.
;; When calling, set outp to 0
;; Or make your own helper.
(define (letter-count-r the-sent outp)
  (cond [(empty? the-sent) outp]
        [else (letter-count-r (butfirst the-sent) 
                              (+ outp (count (first the-sent))))]))

;; 14.6  Write member?.
;; This looks like accumulate
(define (member-r? the-word the-sent)
  (member-r-helper the-word the-sent #f))

(define (member-r-helper the-word the-sent outp)
  (cond [(empty? the-sent) outp]
        [(equal? the-word (first the-sent)) (member-r-helper the-word '() #t)]
        [else (member-r-helper the-word (butfirst the-sent) #f)]))

;; 14.7  Write differences, which takes a sentence of numbers as its argument 
;; and returns a sentence containing the differences between adjacent elements. 
;; (The length of the returned sentence is one less than that of the argument.)
;; So we do not go all the way through.
;; Sort of like every
(define (differences-r the-nums outp)
  (cond [(equal? (count the-nums) 1) outp]
        [else (differences-r (butfirst the-nums) 
                             (sentence outp 
                                       (- (more:simply-second the-nums) 
                                          (first the-nums))))]))

;; 14.8  Write expand, which takes a sentence as its argument. 
;; It returns a sentence similar to the argument, 
;; except that if a number appears in the argument, 
;; then the return value contains that many copies of the following word:
;; I will have to make a helper function, that can also be recursive.
;; But this looks like every.
(define (print-n-times the-num the-word outp)
  (cond [(equal? 0 the-num) outp]
        [else (print-n-times (- the-num 1) the-word (sentence the-word outp))]))

(define (expand-r the-sent outp)
  (cond [(equal? (count the-sent) 0) outp]
        [(number? (first the-sent)) 
         (expand-r (butfirst (butfirst the-sent)) 
                   (sentence outp (print-n-times (first the-sent) 
                                                 (more:simply-second the-sent) 
                                                 '())))]
        [else (expand-r (butfirst the-sent) 
                        (sentence outp 
                                  (first the-sent)))]))

;; 14.9  Write a procedure called location that takes two arguments, a word and a sentence. 
;; It should return a number indicating where in the sentence that word can be found. 
;; If the word isn't in the sentence, return #f. 
;; If the word appears more than once, return the location of the first appearance.
;; Shouldn't it return 0 if the word is not found? 
;; I don't like the idea that it returns a number OR a boolean.
;; That kind of goes against his advice in chapter 12.
;; Sort of like accumulate, but like member? you do not have to go all the way through.
(define (location the-word sent)
  (location-r the-word sent 0 1))

(define (location-r the-word sent outp counter)
  (cond [(equal? (count sent) 0) outp]
        [(equal? the-word (first sent)) (location-r the-word 
                                                    '() 
                                                    counter 
                                                    counter)]
        [else (location-r the-word 
                          (butfirst sent) 
                          outp 
                          (+ counter 1))]))

;; 14.10  Write the procedure count-adjacent-duplicates that takes a sentence as an argument 
;; and returns the number of words in the sentence that are immediately followed by the same word:
;; I think this is like accumulate
;; (count-adjacent-dups-r '(y a b b a d a b b a d o o) 0)
(define (count-adjacent-dups-r the-sent outp)
  (cond [(equal? (count the-sent) 1) outp]
        [(equal? (first the-sent) (more:simply-second the-sent)) (count-adjacent-dups-r (butfirst the-sent) (+ 1 outp))]
        [else (count-adjacent-dups-r (butfirst the-sent) outp)]))

;; 14.11  Write the procedure remove-adjacent-duplicates that takes a sentence as argument 
;; and returns the same sentence but with any word that's immediately followed by the same word removed:
;; This is like keep
;; (remove-adj-dups-r '(y a b b a d a b b a d o o) '())
;; (remove-adj-dups-r '(yeah yeah yeah) '())
(define (remove-adj-dups-r the-sent outp)
  (cond [(equal? (count the-sent) 0) outp]
        [(and (equal? (count the-sent) 1) 
              (equal? (first the-sent) (last outp))) outp]
        [(equal? (first the-sent) (more:simply-second the-sent)) 
         (remove-adj-dups-r (butfirst (butfirst the-sent)) 
                            (sentence outp (first the-sent)))]
        [else (remove-adj-dups-r (butfirst the-sent) 
                                 (sentence outp (first the-sent)))]))

;; 14.12  Write a procedure progressive-squares? that takes a sentence of numbers as its argument. 
;; It should return #t if each number (other than the first) is the square of the number before it:
;; This is accumulate
(define (progressive-squares? the-sent)
  (prog-sqrs? the-sent #t))

(define (prog-sqrs? the-sent outp)
  (cond [(or (equal? (count the-sent) 0) (equal? (count the-sent) 1)) outp]
        [(equal? (more:square (first the-sent)) (more:simply-second the-sent)) (prog-sqrs? (butfirst the-sent) #t)]
        [else (prog-sqrs? '() #f)]))

;; 14.13  What does the pigl procedure from Chapter 11 do if you invoke it with a word like "frzzmlpt" that has no vowels? 
;; It goes in an infinite loop
;; Fix it so that it returns "frzzmlptay."
; (define (pigl wd)
;   (if (member? (first wd) 'aeiou)
;       (word wd 'ay)
;      (pigl (word (bf wd) (first wd)))))

(define (all-consonants? the-word)
  (if (zero? (count (keep more:vowel? the-word)))
      #t
      #f))

(define (remove-char the-char the-string)
  (keep (lambda (x) (not (equal? the-char x))) the-string))

(define (pigl wd)
  (remove-char '_ (pigl-r wd '_)))

(define (pigl-r the-word outp)
  (cond [(all-consonants? the-word) (word the-word 'ay)]
        [(member? (first the-word) 'aeiou) (word the-word outp 'ay)]
        [else (pigl-r (butfirst the-word) (word outp (first the-word)))]))
;; I guess this is keep

;; 14.14  Write a predicate same-shape? that takes two sentences as arguments. 
;; It should return #t if two conditions are met: 
;; The two sentences must have the same number of words, 
;; and each word of the first sentence must have the same number of letters as the word in the corresponding position in the second sentence.
;; not tail-recursive
(define (same-shape? first-sent second-sent outp)
  ; (display-all "calling same-shape? with first-sent: " first-sent ", second-sent: " second-sent ", outp: " outp)
  (cond [(not (equal? (count first-sent) (count second-sent))) #f]
        [(and (empty? first-sent) (empty? second-sent)) outp]
        [(equal? (count (first first-sent)) 
                 (count (first second-sent))) 
         (same-shape? (butfirst first-sent) (butfirst second-sent) #t)]
        [else #f]))
;; This is accumulate

;; 14.15  Write merge, a procedure that takes two sentences of numbers as arguments. 
;; Each sentence must consist of numbers in increasing order. 
;; Merge should return a single sentence containing all of the numbers, in order. 
;; (We'll use this in the next chapter as part of a sorting algorithm.)
;; > (merge '(4 7 18 40 99) '(3 6 9 12 24 36 50))
;; (3 4 6 7 9 12 18 24 36 40 50 99)
;; I will just assume that all of the numbers are already sorted
;; And that there are no numbers in both lists
;; Two into one: accumulate
;; (merge-r '(4 7 18 40 99) '(3 6 9 12 24 36 50) '())
;; not tail recursive, lots of conditions
(define (merge-r nums-a nums-b outp)
  (cond [(and (empty? nums-a) (empty? nums-b)) outp]
        [(empty? nums-a) (sentence outp nums-b)]
        [(empty? nums-b) (sentence outp nums-a)]
        [(< (first nums-a) (first nums-b)) (merge-r (butfirst nums-a) 
                                                    nums-b 
                                                    (sentence outp (first nums-a)))]
        [(< (first nums-b) (first nums-a)) (merge-r nums-a 
                                                    (butfirst nums-b) 
                                                    (sentence outp (first nums-b)))]
        [else outp]))

;; 14.16  Write a procedure syllables that takes a word as its argument and returns the number of syllables in the word, 
;; counted according to the following rule: 
;; the number of syllables is the number of vowels, 
;; except that a group of consecutive vowels counts as one. 
;; For example, in the word "soaring," 
;; the group "oa" represents one syllable and the vowel "i" represents a second one.

;; Be sure to choose test cases that expose likely failures of your procedure. 
;; For example, what if the word ends with a vowel? 
;; What if it ends with two vowels in a row? What if it has more than two consecutive vowels?

;; Of course this rule isn't good enough. 
;; It doesn't deal with things like silent "e"s that don't create a syllable ("like"), 
;; consecutive vowels that don't form a diphthong ("cooperate"), 
;; letters like "y" that are vowels only sometimes, etc. 
;; If you get bored, see whether you can teach the program to recognize some of these special cases.
;; collection to one answer: accumulate
;; I think other languages would not have those problems.
;; German: It's the same, every time.
;; And Finn talked about Gaelic having long and short vowels, and they always follow a pattern.
;; call like this: (syllables-r 'some-word 0)
(define (syllables-r the-word outp)
  ; (display-all "calling syllables-r with the-word: " the-word ", outp: " outp)
  (cond [(or (empty? the-word) (equal? (count the-word) 1)) outp]
        [(and (more:vowel? (first the-word)) 
              (not (more:vowel? (first (butfirst the-word))))) 
         (syllables-r (butfirst the-word) (+ 1 outp))]
        [(and (not (more:vowel? (first the-word))) 
              (more:vowel? (first (butfirst the-word))))
         (syllables-r (butfirst the-word) (+ 1 outp))]
        [else (syllables-r (butfirst the-word) outp)]))

(module+ test
  (require rackunit)
  (check-true #t)
  
  ;; 14.01
  (printf "(remove-once-r 'morning '(good morning good morning) '()): ~a \n" (remove-once-r 'morning '(good morning good morning) '()))
  (check-equal? (remove-once-r 'morning '(good morning good morning) '()) '(good good morning) "Error for: (remove-once-r 'morning '(good morning good morning) '())")

  ;; 14.02
  (printf "(up-r 'town '()): ~a \n" (up-r 'town '()))
  (check-equal? (up-r 'town '()) '(t to tow town) "Error for: (up-r 'town '())")

  ;; 14.03
  (printf "(remove-dup-r '(ob la di ob la da) '()): ~a \n" (remove-dup-r '(ob la di ob la da) '()))
  (check-equal? (remove-dup-r '(ob la di ob la da) '()) '(ob la di da) "Error for: (remove-dup-r '(ob la di ob la da) '())")

  ;; 14.04
  (printf "(odds '(i lost my little girl)): ~a \n"  (odds '(i lost my little girl)))
  (check-equal? (odds '(i lost my little girl)) '(i my girl)  "Error for: (odds '(i lost my little girl))")

  ;; 14.05
  (printf "(letter-count-r '(fixing a hole) 0): ~a \n" (letter-count-r '(fixing a hole) 0))
  (check-equal? (letter-count-r '(fixing a hole) 0) 11 "Error for: (letter-count-r '(fixing a hole) '())")

  ;; 14.06
  (printf "(member-r? 'what '(ask not what your country can do for you)): ~a \n" (member-r? 'what '(ask not what your country can do for you)))
  (check-equal? (member-r? 'what '(ask not what your country can do for you)) #t  "Error for: (member-r? 'what '(ask not what your country can do for you))")
  (printf "(member-r? 'when '(ask not what your country can do for you)): ~a \n" (member-r? 'when '(ask not what your country can do for you)))
  (check-equal? (member-r? 'when '(ask not what your country can do for you)) #f "Error for: (member-r? 'when '(ask not what your country can do for you))")
;; > 

  ;; 14.07 
  (printf "(differences-r '(4 23 9 87 6 12) '()): ~a \n" (differences-r '(4 23 9 87 6 12) '()))
  (check-equal? (differences-r '(4 23 9 87 6 12) '()) '(19 -14 78 -81 6) "Error for: (differences-r '(4 23 9 87 6 12) '())")

  ;; 14.08
  (printf "(expand-r '(4 calling birds 3 french hens) '()) '(): ~a \n" (expand-r '(4 calling birds 3 french hens) '()) )
  (check-equal? (expand-r '(4 calling birds 3 french hens) '()) 
                '(calling calling calling calling birds french french french hens)  
                "Error for: (expand-r '(4 calling birds 3 french hens) '())")
  (printf "(expand-r '(the 7 samurai) '()): ~a \n" (expand-r '(the 7 samurai) '()))
  (check-equal? (expand-r '(the 7 samurai) '()) 
                '(the samurai samurai samurai samurai samurai samurai samurai) 
                "Error for: (expand-r '(the 7 samurai) '())")

  ;; 14.09
  (printf "(location 'me '(you never give me your money)): ~a \n" (location 'me '(you never give me your money)))
  (check-equal? (location 'me '(you never give me your money)) 
                4  
                "Error for: (location 'me '(you never give me your money))")

  ;; 14.10
  (printf "(count-adjacent-dups-r '(y a b b a d a b b a d o o) 0): ~a \n" (count-adjacent-dups-r '(y a b b a d a b b a d o o) 0) )
  (check-equal? (count-adjacent-dups-r '(y a b b a d a b b a d o o) 0) 
                3  
                "Error for: (count-adjacent-dups-r '(y a b b a d a b b a d o o) 0)")
  (printf "(count-adjacent-dups-r '(yeah yeah yeah) 0): ~a \n" (count-adjacent-dups-r '(yeah yeah yeah) 0) )
  (check-equal? (count-adjacent-dups-r '(yeah yeah yeah) 0) 
                2  
                "Error for: (count-adjacent-dups-r '(yeah yeah yeah) 0)")

  ;; 14.11
  (printf "(remove-adj-dups-r '(y a b b a d a b b a d o o) '()): ~a \n" (remove-adj-dups-r '(y a b b a d a b b a d o o) '()))
  (check-equal? (remove-adj-dups-r '(y a b b a d a b b a d o o) '()) 
                '(y a b a d a b a d o) 
                "Error for: (remove-adj-dups-r '(y a b b a d a b b a d o o) '())")
  (printf "(remove-adj-dups-r '(yeah yeah yeah) '()): ~a \n" (remove-adj-dups-r '(yeah yeah yeah) '()))
  (check-equal? (remove-adj-dups-r '(yeah yeah yeah) '()) 
                '(yeah) 
                "Error for: (remove-adj-dups-r '(yeah yeah yeah) '())")

  ;; 14.12
  (printf "(progressive-squares? '(3 9 81 6561)): ~a \n"(progressive-squares? '(3 9 81 6561)) )
  (check-equal? (progressive-squares? '(3 9 81 6561)) #t "Error for: (progressive-squares? '(3 9 81 6561))")
  (printf "(progressive-squares? '(25 36 49 64)): ~a \n" (progressive-squares? '(25 36 49 64)))
  (check-equal? (progressive-squares? '(25 36 49 64)) #f "Error for: (progressive-squares? '(25 36 49 64))")

  ;; 14.13
  (printf "(pigl 'frzzmlpt): ~a \n" (pigl 'frzzmlpt))
  (check-equal? (pigl 'frzzmlpt) 'frzzmlptay "Error for: (pigl 'frzzmlpt)")
  (printf "(pigl 'proper): ~a \n" (pigl 'proper))
  (check-equal? (pigl 'proper) 'operpray "Error for: (pigl 'proper)")

  ;; 14.14
  (printf "(same-shape? '(the fool on the hill) '(you like me too much) #f): ~a \n"
          (same-shape? '(the fool on the hill) '(you like me too much) #f))
  (check-equal? (same-shape? '(the fool on the hill) '(you like me too much) #f) 
                #t 
                "Error for: (same-shape? '(the fool on the hill) '(you like me too much) #f)")
  (printf "(same-shape? '(the fool on the hill) '(and your bird can sing) #f): ~a \n" (same-shape? '(the fool on the hill) '(and your bird can sing) #f)  )
  (check-equal? (same-shape? '(the fool on the hill) '(and your bird can sing) #f) 
                #f 
                "Error for: (same-shape? '(the fool on the hill) '(and your bird can sing) #f)  ")

  ;; 14.15
  (printf "(merge-r '(4 7 18 40 99) '(3 6 9 12 24 36 50) '()): ~a \n" (merge-r '(4 7 18 40 99) '(3 6 9 12 24 36 50) '()))
  (check-equal? (merge-r '(4 7 18 40 99) '(3 6 9 12 24 36 50) '()) 
                '(3 4 6 7 9 12 18 24 36 40 50 99) 
                "Error for: (merge-r '(4 7 18 40 99) '(3 6 9 12 24 36 50) '())")
) ;; end module+ test 



