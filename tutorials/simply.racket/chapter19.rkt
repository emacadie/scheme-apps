#lang simply-scheme

; Chapter 19 Higher Order Functions

(require "more-simply.rkt")
(require (prefix-in ch17: "chapter17.rkt"))


(butfirst '(This is chapter 19 Higher Order Functions))

;; Chapter 19 Higher Order Functions

;; from chapter 14, tail-recursive
;; (every-something-r (lambda (x) (* 2 x)) '(1 2 3) '())
(define (my-map func sent)
  (my-map-r func sent '()))
(define (my-map-r func sent outp)
  (cond [(null? sent) (reverse outp)]
        [else (my-map-r func 
                        (cdr sent) 
                        (cons (func (car sent)) outp))]))

#| 
(define (map fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst)) (map fn (cdr lst)))))
|#

(define (every2 func sent outp)
  (cond [(null? sent) outp]
        [(empty? outp) (every2 func 
                               (cdr sent) 
                               (list (func (car sent))))]
        [else (begin
                (printf "in else, with car sent: ~a \n" (car sent))
                (every2 func 
                        (cdr sent) 
                        (ch17:my-append outp 
                                        (func (car sent)))))]))

; from chapter 14 - tail recursive
(define (my-keep predicate sent)
  (keep-r predicate sent '()))
(define (keep-r predicate sent outp)
  (cond [(empty? sent) outp] 
        [(and (predicate (car sent)) (empty? outp)) (keep-r predicate 
                                                            (cdr sent)
                                                            (list (car sent)))]
	    [(predicate (car sent)) (keep-r predicate 
                                        (cdr sent) 
                                        (ch17:my-append outp 
                                                        (car sent)))]
	    [else (keep-r predicate 
                      (cdr sent) 
                      outp)]))

; from chapter 14, tail-recursive
(define (my-reduce func start sent)
  (cond [(empty? sent) start]
        [else (my-reduce func
                         (func start (car sent))
                         (cdr sent) )]))


;; 19.2  Write keep. Don't forget that keep has to return a sentence if 
;; its second argument is a sentence, and a word if its second argument is a word.

;; (Hint: it might be useful to write a combine procedure 
;; that uses either word or sentence depending on the types of its arguments.) 
;; from chapter 14
(define (keep-ch19 the-pred sent)
  (cond [(word? sent) (reduce word (keep-ch19-work the-pred sent))]
        [else (keep-ch19-work the-pred sent)]))

(define (keep-ch19-work the-pred sent)
  (cond [(empty? sent) '()]
        [(the-pred (first sent)) (se (first sent) 
                                     (keep-ch19-work the-pred 
                                                     (bf sent)))]
        [else (keep-ch19-work the-pred 
                              (bf sent))]))

;; 19.3  Write the three-argument version of accumulate that we described.
;;> (three-arg-accumulate + 0 '(4 5 6))
;;15
;;> (three-arg-accumulate + 0 '())
;;0
;;> (three-arg-accumulate cons '() '(a b c d e))
;;(A B C D E)
;; my-reduce works for the first two. Try the accumulate from chapter 14

;; I have non-tail recursive solutions
(define (three-arg-accumulate func start sent)
  (if (empty? sent)
      start
      (func (first sent) (three-arg-accumulate func start (bf sent)))))

;; need to flatten the result for the last one
(define (three-arg-accumulate-r func start sent)
  (cond [(empty? sent) start]
        [else (three-arg-accumulate-r func
                         (func start (first sent))
                         (bf sent) )]))

; 19.4  Our accumulate combines elements from right to left. That is,
; (accumulate - '(2 3 4 5))
; computes 2−(3−(4−5)). 
; Write left-accumulate, which will compute ((2−3)−4)−5 instead. 
; (The result will be the same for an operation such as +, 
; for which grouping order doesn't matter, but will be different for -.) 

(define (left-accumulate func sent)
  (cond [(= (count sent) 1) (first sent)]
        [else (func (left-accumulate func (butlast sent)) (last sent))]))

;; If I do (left-accumulate - '(2 3 4 5)) I get -14
;; If I do (- (- (- 2 3) 4) 5) I get -10
;; Should 0 be the base case?
;; Let's see other solutions
;; try with last instead of first, based on buntine
;; I still get -14
;; from https://github.com/hosoe-masaki/SimplyScheme/blob/master/ch19/19-4.md
;; don't make empty your base case, make it count = 1 and send the first
#|
Here is what almost worked 
(define (left-accumulate func sent)
  (printf "calling left-accumulate with sent: ~a \n" sent)
  (cond [(empty? sent) 0] 
        [else (func (left-accumulate func (bf sent)) (first sent))]))
I suppose you could make a helper func that calls a three-arg with (first sent) as the starting value and (bf sent) as the sentence
|#

;; 19.5  Rewrite the true-for-all? procedure from Exercise 8.10. 
;; Do not use every, keep, or accumulate. 
;; I assume they do not want to use map, filter or reduce
;; Recursion? 
(define (true-for-all-19? pred the-sent)
  (printf "calling true-for-all-19? with the-sent: ~a \n" the-sent)
  (cond [(empty? the-sent) #t]
        [(pred (car the-sent)) (true-for-all-19? pred (cdr the-sent))]
        [else #f]))
;; this works, but what was the point? To replace HOF? I thought the point of this chapter was to use HOF
;; Or make an HOF?
;; What did others do?
;; ** pause while I check other answers **
;; buntine re-implemented keep, others did recursion

;; 19.6  Write a procedure true-for-any-pair? 
;; that takes a predicate and a sentence as arguments. 
;; The predicate must accept two words as its arguments. 
;; Your procedure should return #t if the argument predicate 
;; will return true for any two adjacent words in the sentence:
;> (true-for-any-pair? equal? '(a b c b a))
;#F
;> (true-for-any-pair? equal? '(a b c c d))
;#T
;> (true-for-any-pair? < '(20 16 5 8 6))      
;; 5 is less than 8
;#T
;; https://github.com/pongsh/simply-scheme-exercises/blob/master/19-implementing-higher-order-functions/19.6.scm 
;; is a lot shorter. I thought about this, but there are no higher-order functions.
;; https://github.com/buntine/Simply-Scheme-Exercises/blob/master/19-implementing-higher-order-functions/19-6.scm also shorter, but again no HOF
;; I re-did keep/filter since I have to send two args each time, not just one
;; and I made a function to turn the list into a list of pairs
(define (create-pairs-from-list the-list)
  (create-pairs (first the-list) (butfirst the-list) '()))

(define (create-pairs the-first the-rest outp)
  (cond [(empty? the-rest) (reverse outp)]
        [else (create-pairs (first the-rest) 
                            (butfirst the-rest) 
                            (cons (list the-first (first the-rest)) outp))]))

(define (use-pred pred pair-of-items)
  (pred (first pair-of-items) (last pair-of-items)))

(define (keep-with-pairs predicate sent outp)
  (cond [(empty? sent) outp] 
        [(and (use-pred predicate (first sent)) (empty? outp)) (keep-with-pairs predicate 
                                                                                (butfirst sent)
                                                                                (list (first sent)))]
	    [(use-pred predicate (first sent)) (keep-with-pairs predicate 
                                                            (cdr sent) 
                                                            (append outp (car sent)))]
	    [else (keep-with-pairs predicate 
                               (butfirst sent) 
                               outp)]))

;; b for boolean
(define (keep-with-pairs-b predicate sent outp)
  ; (printf "keep-with-pairs-b with sent: ~a and outp: ~a \n" sent outp)
  (cond [(empty? sent) outp] 
        [(empty? outp)(and ) (keep-with-pairs-b predicate 
                                                                                (butfirst sent)
                                                                                (list (use-pred predicate (first sent))))]
	    [(use-pred predicate (first sent)) (keep-with-pairs-b predicate 
                                                            (cdr sent) 
                                                            (ch17:my-append outp #t))]
	    [else (begin
                ; (printf "In the else\n")
                (keep-with-pairs-b predicate 
                               (butfirst sent) 
                               (ch17:my-append outp #f))
               )]))

#|
(define (true-for-any-pair? pred the-list) 
  (cond [(equal? 0 
                 (count (keep-with-pairs pred 
                                         (create-pairs-from-list the-list) 
                                         '()))) 
         #f]
        [else #t]))
|#
; member? won't work with a boolean
(define (sentence-contains? thing the-sent)
  ; (printf "sentence-contains? with thing: ~a the-sent: ~a \n" thing the-sent)
  (cond [(empty? the-sent) #f]
        [(equal? thing (first the-sent)) #t]
        [else (sentence-contains? thing (butfirst the-sent))]
)
)

(define (true-for-any-pair? pred the-list) 
  (cond [(sentence-contains? #t 
                             (keep-with-pairs-b pred 
                                                (create-pairs-from-list the-list) 
                                                '())) 
         #t]
        [else #f]))

;; 19.7  Write a procedure true-for-all-pairs? 
;; that takes a predicate and a sentence as arguments. 
;; The predicate must accept two words as its arguments. 
;; Your procedure should return #t if the argument predicate 
;; will return true for every two adjacent words in the sentence:
(define (true-for-all-pairs? pred the-list)
  (cond [(sentence-contains? #f
                             (keep-with-pairs-b pred 
                                                (create-pairs-from-list the-list) 
                                                '())) 
         #f]
        [else #t]))
; > (true-for-all-pairs? equal? '(a b c c d))
; #F
; > (true-for-all-pairs? equal? '(a a a a a))
; #T
; > (true-for-all-pairs? < '(20 16 5 8 6))
; #F
; > (true-for-all-pairs? < '(3 7 19 22 43))
; #T


(module+ test
  (require rackunit)
  (check-true #t)
  (define (check-three-things-equal? result their-append-rsl my-append-rsl)
  (unless (and (check-equal? result their-append-rsl)
               (check-equal? result my-append-rsl))
    (fail-check)))
  (check-three-things-equal? (my-map (lambda (x) (* 2 x)) '(1 2 3) ) 
                             (map (lambda (x) (* 2 x)) '(1 2 3) ) 
                             '(2 4 6))
  (check-three-things-equal? (my-map (lambda (n) (expt n n)) '(1 2 3 4 5))
                             (map (lambda (n) (expt n n)) '(1 2 3 4 5))
                             '(1 4 27 256 3125))
  (check-three-things-equal? (my-keep even? '(1 2 3 4 5 6)) 
                             (filter even? '(1 2 3 4 5 6))
                             '(2 4 6))
  (check-three-things-equal? (my-reduce max 0 '(23 54 45 85 65)) 
                             (reduce max '(23 54 45 85 65))
                             85)
  (check-three-things-equal? (my-reduce * 1 '(1 2 3 4 5))
                             (reduce * '(1 2 3 4 5))
                             120)

  (check-equal? 'ei (keep-ch19 vowel? 'qerti))
  (check-equal? '(e u) (keep-ch19 vowel? '(q w e r t u)))

  ; 19.3
  (check-equal? (three-arg-accumulate + 0 '(4 5 6))          15)
  (check-equal? (three-arg-accumulate + 0 '())               0)
  (check-equal? (three-arg-accumulate cons '() '(a b c d e)) '(a b c d e))

  ; 19.4
  (check-equal? (left-accumulate - '(2 3 4 5)) -10)

  ; 19.6
  (check-equal? (true-for-any-pair? equal? '(a b c b a)) #f)
  (check-equal? (true-for-any-pair? equal? '(a b c c d)) #t)
  (check-equal? (true-for-any-pair? < '(20 16 5 8 6))    #t)

  ; 19.7
  (check-equal? (true-for-all-pairs? equal? '(a b c c d)) #f)
  (check-equal? (true-for-all-pairs? equal? '(a a a a a)) #t)
  (check-equal? (true-for-all-pairs? < '(20 16 5 8 6)) #f)
  (check-equal? (true-for-all-pairs? < '(3 7 19 22 43)) #t)


) ;; end module+ test 
  ; (printf ": ~a \n"  )
  ; (check-equal?  "Error for: ")
