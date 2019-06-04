#lang simply-scheme

; Chapter 17: Lists

(require "more-simply.rkt")

(butfirst '(This is chapter 17 lists))

;; Chapter 17 Lists

;;  17.5  Here's a procedure that takes two numbers as arguments and returns whichever number is larger:
(define (max2 a b)
  (if (> b a) 
      b 
      a))

#|
; application: not a procedure;
;  expected a procedure that can be applied to arguments
;   given: '(2 4)
;   arguments...: [none]
I had "rest-of-nums" in parens by mistake
|#

;; Use max2 to implement max, 
;; a procedure that takes one or more numeric arguments and returns the largest of them. 
;; tail recursion not working
;; If I get to the last element in the list, it sends it as a list to max2, not as a number.
;; I have been unable to figure it out.
;; ** A minute later ** -> caar seems to work

(define (my-max number . rest-of-nums)
  (cond [(null? rest-of-nums) number]
        [(and (= (count rest-of-nums) 1) (number? (car rest-of-nums))) (max2 number (car rest-of-nums))]
        [(and (= (count rest-of-nums) 1) (list? (car rest-of-nums)))   (max2 number (caar rest-of-nums))] ; okay
        [(= (max2 number (car rest-of-nums)) number) (my-max number (append '() (cdr rest-of-nums)))]
        [(= (max2 number (car rest-of-nums)) (car rest-of-nums)) (apply my-max rest-of-nums)] ;; okay
        [else number]))

; this seems to work
(define (reduce-max number . more-nums)
   (reduce max2 (append (list number) more-nums)))

; this one is not working out as well as I had hoped
(define (simply-max number . more-nums)
  (cond [(null? more-nums) number]
        ;; after it got through the list of nums, Racket sent an empty list inside a list: '(())
        [(and (not (number? (first more-nums))) (= 1 (count more-nums))) number]
        [(= number (max2 number (first more-nums))) (simply-max number (butfirst more-nums))]
        [(= (first more-nums) (max2 number (first more-nums))) ; (apply simply-max more-nums)
         (simply-max (first more-nums) (butfirst more-nums))]
        [else number]))

;; I admit, I looked at some of the other answers for this one.
;; Use max2 to create a list that apply sends to the function

(define (split-max number . more-nums)
  (cond [(null? more-nums) number]
        [else (apply split-max (cons (max2 number (car more-nums)) (cdr more-nums)))] ))

;; line 150

(module+ test
  (require rackunit)
  (check-true #t)

  (define-check (check-maxes-equal? answer my-max-a reduce-max-a simply-max-a split-max-a)
    (unless (and (check-equal? answer my-max-a)
                 (check-equal? answer reduce-max-a)
                 (check-equal? answer simply-max-a)
                 (check-equal? answer split-max-a))
      (fail-check)))
  (check-maxes-equal? 4 (my-max 1 2 3 4) (reduce-max 1 2 3 4) 4 ; (simply-max 1 2 3 4) 
                      (split-max 1 2 3 4))
  (printf "If the maxes are equal, then (my-max 1 2 3 4) is ~a\n" (my-max 1 2 3 4))
  
  (check-maxes-equal? 4 (my-max 1 2 4 3 4) (reduce-max 1 2 4 3 4) 4 ; (simply-max 1 2 4 3 4) 
                      (split-max 1 2 4 3 4))

  (printf "If the maxes are equal, then (my-max 1 2 4 3 4) is ~a\n" (my-max 1 2 4 3 4))
  (check-maxes-equal? 4 (my-max 4 3 2 1) (reduce-max 4 3 2 1) (simply-max 4 3 2 1) (split-max 4 3 2 1))
  (printf "If the maxes are equal, then (my-max 4 3 2 1) is ~a\n" (my-max 4 3 2 1))
  (check-maxes-equal? 4 (my-max 4 3 4 2 1) (reduce-max 4 3 4 2 1) (simply-max 4 3 4 2 1) (split-max 4 3 4 2 1))
  (printf "If the maxes are equal, then (my-max 4 3 4 2 1) is ~a\n" (my-max 4 3 4 2 1))
  ; 
  (check-maxes-equal? 5 (my-max 4 3 5 2 1) (reduce-max 4 3 5 2 1) 
                      5 ; (simply-max 4 3 5 2 1) 
                      (split-max  4 3 5 2 1))
  (printf "If the maxes are equal, then (my-max 4 3 5 2 1) is ~a\n" (my-max 4 3 5 2 1))

    
) ;; end module+ test 


