;; Chapter 15 Advanced Recursion

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
  (display-all "get-all-powers-of-2 with the-num: " the-num ", outp: " outp)
  (cond ((equal? the-num 0) 
         (display-all "in first cond of get-all-powers-of-2")
         outp)
        ((equal? the-num 2) (sentence 1 outp))
        (else
         (display-all "in else for get-all-powers-of-2")
         (get-all-powers-of-2 (- the-num (expt 2 (closest-power-of-2 the-num 0))) 
                              (sentence (+ (closest-power-of-2 the-num 0) 1) outp)))))

(define (to-binary-r power-list counter outp)
  (display-all "in to-binary-r with power-list: " power-list ", counter: " counter ", outp: " outp)
  (cond ((empty? power-list) outp)
        ((equal? (first power-list) counter) (to-binary-r (butfirst power-list) (+ 1 counter) (sentence 1 outp)))
        (else (to-binary-r power-list (+ 1 counter) (sentence 0 outp)))))
;; call this
(define (to-binary dec-num)
  (to-binary-r (get-all-powers-of-2 (trim-leading-zeros dec-num) '()) 
               1
               '()))
;; I think this works

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


