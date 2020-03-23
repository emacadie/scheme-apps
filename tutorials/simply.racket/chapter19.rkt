#lang simply-scheme

; Chapter 19 Higher Order Functions

(require (prefix-in more: "more-simply.rkt")
         (prefix-in ch17: "chapter17.rkt"))


(provide sort-19-list)

(butfirst '(This is chapter 19 Higher Order Functions))

;; Chapter 19 Higher Order Functions

#|
from chapter 17:
" We'll use the name structured list for a list that includes sublists."
(Isn't that also how they defined trees?)
Anyway, they mention you can call higher-order functions on structured lists:
(define (deep-map f structure)
  (cond [(word? structure) (f structure)]
        [(null? structure) '()]
        [else (cons (deep-map f (car structure))
                    (deep-map f (cdr structure)))]))
Map on the car and map on the cdr, just like trees.
Not tail-recursive.
|#

;; code for map, reduce and filter at
;; http://people.cs.aau.dk/~normark/prog3-03/html/notes/higher-order-fu_themes-map-filter-section.html
;; and
;; http://people.cs.aau.dk/~normark/prog3-03/html/notes/higher-order-fu_themes-reduction-zip-section.html
;; but not tail-recursive (at least their map is not)
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
                ; (printf "in else, with car sent: ~a \n" (car sent))
                (every2 func 
                        (cdr sent) 
                        (ch17:my-append outp 
                                        (func (car sent)))))]))

; from chapter 14 - tail recursive
(define (my-keep predicate sent)
  (keep-r predicate sent '()))
(define (keep-r predicate sent outp)
  (cond [(empty? sent) (reverse outp)] 
        [(and (predicate (car sent)) (empty? outp)) (keep-r predicate 
                                                            (cdr sent)
                                                            (list (car sent)))]
	    [(predicate (car sent)) (keep-r predicate 
                                        (cdr sent) 
                                        (cons (car sent) outp))]
	    [else (keep-r predicate (cdr sent) outp)]))

; from chapter 14, tail-recursive
(define (my-reduce func start sent)
  ; (printf "calling my-reduce with start: ~a and send: ~a \n" start sent)
  (cond [(empty? sent) start]
        [else (my-reduce func
                         (func start (car sent))
                         (cdr sent))]))


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

;; I hate non-tail recursive solutions
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
;; https://github.com/buntine/Simply-Scheme-Exercises/blob/master/19-implementing-higher-order-functions/19-6.scm 
;; also shorter, but again no HOF
;; I re-did keep/filter since I have to send two args each time, not just one
;; and I made a function to turn the list into a list of pairs
(define (create-pairs-from-list the-list)
  (create-pairs (car the-list) (cdr the-list) '()))

(define (create-pairs the-first the-rest outp)
  (cond [(empty? the-rest) (reverse outp)]
        [else (create-pairs (car the-rest) 
                            (cdr the-rest) 
                            (cons (list the-first (car the-rest)) outp))]))
; "car" = "first", "cdr = "butfirst", but what is "last"?
(define (use-pred pred pair-of-items)
  (pred (first pair-of-items) (last pair-of-items)))

(define (keep-with-pairs predicate sent outp)
  ; (printf "keep-with-pairs-b with sent: ~a and outp: ~a \n" sent outp)
  (cond [(empty? sent) outp] 
        [(empty? outp) (keep-with-pairs predicate 
                                        (cdr sent)
                                        (list (use-pred predicate (car sent))))]
	    [(use-pred predicate (car sent)) (keep-with-pairs predicate 
                                                          (cdr sent) 
                                                          (ch17:my-append outp #t))]
	    [else (begin
                ; (printf "In the else\n")
                (keep-with-pairs predicate 
                                 (cdr sent) 
                                 (ch17:my-append outp #f)))]))

; member? won't work with a boolean
(define (sentence-contains? thing the-sent)
  ; (printf "sentence-contains? with thing: ~a the-sent: ~a \n" thing the-sent)
  (cond [(empty? the-sent) #f]
        [(equal? thing (first the-sent)) #t]
        [else (sentence-contains? thing (butfirst the-sent))]))

(define (true-for-any-pair? pred the-list) 
  (cond [(sentence-contains? #t 
                             (keep-with-pairs pred 
                                              (create-pairs-from-list the-list) 
                                              '())) 
         #t]
        [else #f]))

;; 19.7  Write a procedure true-for-all-pairs? 
;; that takes a predicate and a sentence as arguments. 
;; The predicate must accept two words as its arguments. 
;; Your procedure should return #t if the argument predicate 
;; will return true for every two adjacent words in the sentence:

;; This is a lot of work, but I figured we are supposed to use/make HOF, 
;; So I made some HOF
(define (true-for-all-pairs? pred the-list)
  (cond [(sentence-contains? #f
                             (keep-with-pairs pred 
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

;; 19.8  Rewrite true-for-all-pairs? (Exercise 19.7) using true-for-any-pair? (Exercise 19.6) as a helper procedure. 
;; Don't use recursion in solving this problem (except for the recursion you've already used to write true-for-any-pair?). 
;; Hint: You'll find the not procedure helpful.

;; The only way I can think of to do this is to just add a condition to the cond.
;; Is this an easy one? Or am I misunderstanding things?
(define (true-for-all-pairs-19-8? pred the-list)
  (cond [(not (true-for-any-pair? pred the-list)) #f] 
        [(sentence-contains? #f
                             (keep-with-pairs pred 
                                              (create-pairs-from-list the-list) 
                                              '())) 
         #f]
        [else #t]))

;; 19.9  Rewrite either of the sort procedures from Chapter 15 to take two arguments, a list and a predicate. 
;; It should sort the elements of that list according to the given predicate:
; > (sort '(4 23 7 5 16 3) <)
; (3 4 5 7 16 23)
; > (sort '(4 23 7 5 16 3) >)
; (23 16 7 5 4 3)
; > (sort '(john paul george ringo) before?)
; (GEORGE JOHN PAUL RINGO)
;; before? is a Simply Scheme function
; remove-once was exercise 14.1
(define (remove-once-sent wd the-sent)
  (cond [(empty? the-sent) '()]
        [(equal? wd (first the-sent)) (bf the-sent)]
        [else (se (first the-sent) (remove-once-sent wd (bf the-sent)))]))

;; this is from the comments of chapter 15
(define (earliest-word-sent the-sent pred)
  (accumulate (lambda (wd1 wd2) (if (pred wd1 wd2) wd1 wd2))
	      the-sent))

(define (sort-19-sent the-sent pred)
  (cond [(empty? the-sent) '()]
        [else (se (earliest-word-sent the-sent pred)
                  (sort-19-sent (remove-once-sent (earliest-word-sent the-sent 
                                                                      pred) 
                                                  the-sent) 
                                pred))]))

;; now let's try with lists and reduce
(define (remove-once-list wd the-list)
  (cond [(empty? the-list) '()]
        [(equal? wd (car the-list)) (cdr the-list)]
        [else (ch17:flatten2 (list (car the-list) 
                                   (remove-once-list wd (cdr the-list))))]))

;; chapter 14's exercise, with lists
;; it's like calling filter and you stop filtering after the first hit
(define (remove-once-r bad-word inp outp)
  (cond [(empty? inp) outp]
        [(equal? bad-word (car inp)) (append outp (cdr inp))]
        [else (remove-once-r bad-word 
                             (cdr inp) 
                             (append outp (list (car inp))))]))

;; this is from the comments of chapter 15
(define (earliest-word-list the-list pred)
  (reduce (lambda (wd1 wd2) (if (pred wd1 wd2) wd1 wd2))
	      the-list))

(define (sort-19-list-hlpr the-list pred output)
  (cond [(empty? the-list) (reverse output)]
        [else
         (let ([early-word (earliest-word-list the-list pred)])
           (sort-19-list-hlpr (remove-once-r early-word the-list '()) 
                              pred 
                              (cons early-word output)))]))

(define (sort-19-list the-list pred)
  (sort-19-list-hlpr the-list pred '()))

;; 19.10  Write tree-map, analogous to our deep-map, 
; but for trees, using the datum and children selectors. 
(define (deep-map f structure)
  ; (printf "Calling deep-map with structure ~a\n" structure)
  (cond [(word? structure) (f structure)]
        [(null? structure) '()]
        [else (cons (deep-map f (car structure))
                    (deep-map f (cdr structure)))]))

;; I admit, I tried to get it to work with one function,
;; and I peeked at the other solutions, and I had forgotten
;; the cardinal rule of trees:
;; a function for trees, and one for the forest
;; this is some progress
(define (tree-map f structure)
  ; (printf "Calling tree-map with structure: ~a\n" structure)
  (cond [(leaf? structure) 
         (begin
           ; (printf "~a is a leaf\n" structure)
           (make-node (f (datum structure)) '())
           )]
        [(null? structure) '()]
        [else (make-node (f (datum structure))
                    (forest-map f (children structure)))]))

(define (forest-map f structure)
  ; (printf "calling forest-map with structure: ~a\n" structure)
  (cond [(null? structure) null]
        [else (make-node (tree-map f (datum structure))
                         (forest-map f (children structure)))]))

(define (count-leaves tree)
  (if (leaf? tree)
      1
      (count-leaves-in-forest (children tree))))

(define (count-leaves-in-forest forest)
  (if (null? forest)
      0
      (+ (count-leaves (car forest))
         (count-leaves-in-forest (cdr forest)))))

(define (leaf datum)
  (make-node datum '()))
(define (tree-map-meng fn tree)
  (if (leaf? tree)
      (leaf (fn (datum tree)))
      (cons (fn (datum tree))
            (forest-map-meng fn (children tree)))))

(define (forest-map-meng fn forest)
  (if (null? forest)
      '()
      (cons (tree-map-meng fn (car forest))
            (forest-map-meng fn (cdr forest)))))

; 19.11  Write repeated. (This is a hard exercise!) 
;; from chapter 8
;> ((repeated bf 3) '(she came in through the bathroom window))
; (THROUGH THE BATHROOM WINDOW)
;> ((repeated plural 4) 'computer)
;COMPUTERSSSS
; > ((repeated square 2) 3)
; 81
;; works, but does not return a function
;; It's not THAT big of a deal to write the function name
;; a few times, is it?
;; (In the book, they mostly use repeated to define a function at the place it is used)
;; and it's still an HOF
;; I will study some of the other people's answers, and punt on this one
(define (my-repeated func num input)
  (cond [(equal? num 0) input]
        [else (my-repeated func (- num 1) (func input))]))

#|
(define (my-repeated-again func num . func-list)
  (cond [(equal? num 0) func-list]
        [else (my-repeated func (- num 1) (func e))]))
|#
(define (bf3 input)
  (butfirst (butfirst (butfirst input))))
#|
(define (inner-lambda func)
  (lambda (x) (func x))
)
|#

; 19.12  Write tree-reduce. 
;; You may assume that the combiner argument can be invoked with no arguments. 
#|
> (tree-reduce
   +
   (make-node 3 (list (make-node 4 '())
		      (make-node 7 '())
		      (make-node 2 (list (make-node 3 '())
					 (make-node 8 '()))))))
27
|#
; this seems to work
(define (tree-reduce func the-tree)
  (reduce func (ch17:flatten2 the-tree)))

;; make sure reduce works the way I think
;; IE: first arg is the total
(define (add-two a b)
  (printf "adding two nums ~a and ~a to get ~a \n" a b (+ a b) )
  (+ a b))
; I will use this in my tests for tree-reduce
(define (append-first-to-list the-list the-word)
  (cond [(null? the-list) (list (first the-word))]
        [else (ch17:my-append the-list (first the-word))]))

; this is when reduce goes in reverse order of how I think it should
; from end of list to beginning
(define (word-from-first-r word-b word-a)
  (cond [(null? word-a) (first word-b)]
        [(null? word-b) word-a]
        [(not (member? " "  word-a)) 
         (begin
           (word (first word-a) " " (first word-b)))]
        [else (begin
                (word (first word-b) " " word-a))]))

(define (word-from-first word-a word-b)
  (cond [(null? word-a) (first word-b)]
        [(null? word-b) word-a]
        [else (word word-a " " (first word-b))]))

;; look at a few other answers for tree-reduce. 
;; It looks like other answers are pretty long. Am I wrong?
;; https://github.com/buntine/Simply-Scheme-Exercises/blob/master/19-implementing-higher-order-functions/19-12.scm
(define (tree-reduce-buntine func tree)
  (if (null? tree)
    #f
    (func (datum tree) (tree-reduce-in-forest func (children tree)))))
;; I think the no-arg call to func is causing issues
(define (tree-reduce-in-forest func tree)
  (if (null? tree)
    (func) 
    (func (tree-reduce-buntine func (car tree))
          (tree-reduce-in-forest func (cdr tree)))))

;; https://github.com/mengsince1986/Simply-Scheme-exercises/blob/master/SS%20Exercises/Exercises%2019.2-19.13.scm
(define (leaf? node)
  (null? (children node)))

(define (tree-reduce-meng combiner tree)
  (if (leaf? tree)
      (datum tree)
      (combiner (datum tree) (forest-reduce-meng combiner (children tree)))))

(define (forest-reduce-meng combiner forest)
  (if (null? (cdr forest))
      (tree-reduce-meng combiner (car forest))
      (combiner (tree-reduce-meng combiner (car forest))
                (forest-reduce-meng combiner (cdr forest)))))

;; 19.13  Write deep-reduce, similar to tree-reduce, but for structured lists: 
;; I think this can be done like tree-reduce
(define (deep-reduce func deep-list)
  (reduce func (ch17:flatten2 deep-list)))

(module+ test
  (require rackunit)
  (check-true #t)
  (define (check-three-things-equal? result their-append-rsl my-append-rsl)
    (unless (and (check-equal? result their-append-rsl)
                 (check-equal? result my-append-rsl))
      (fail-check)))

  (define (leaf datum)
    (make-node datum '()))

  (define (cities name-list)
    (map leaf name-list))
  (define tiny-world-tree
    (make-node
     'world
     (list (make-node 'zimbabwe (cities '(harare hwange)))
           (make-node
            'australia
            (list
             (make-node 'victoria (cities '(melbourne)))
             (make-node '(new south wales) (cities '(sydney)))
             (make-node 'queensland
                        (cities '(cairns (port douglas)))))))))

  (define num-tree (make-node 3 (list (make-node 4 '())
                                      (make-node 7 '())
                                      (make-node 2 (list (make-node 3 '())
                                                         (make-node 8 '()))))))

  ;; Just testing my-map
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

  ;; 19.02
  (check-equal? 'ei    (keep-ch19 more:vowel? 'qerti))
  (check-equal? '(e u) (keep-ch19 more:vowel? '(q w e r t u)))

  ; 19.03
  (check-equal? (three-arg-accumulate + 0 '(4 5 6))          15)
  (check-equal? (three-arg-accumulate + 0 '())               0)
  (check-equal? (three-arg-accumulate cons '() '(a b c d e)) '(a b c d e))

  ; 19.04
  (check-equal? (left-accumulate - '(2 3 4 5)) -10)

  ; 19.06
  (check-equal? (true-for-any-pair? equal? '(a b c b a))   #f)
  (check-equal? (true-for-any-pair? equal? '(a b c c d))   #t)
  (check-equal? (true-for-any-pair? <      '(20 16 5 8 6)) #t)

  ; 19.07
  (check-equal? (true-for-all-pairs? equal? '(a b c c d))    #f)
  (check-equal? (true-for-all-pairs? equal? '(a a a a a))    #t)
  (check-equal? (true-for-all-pairs? <      '(20 16 5 8 6))  #f)
  (check-equal? (true-for-all-pairs? <      '(3 7 19 22 43)) #t)

  ;; 19.09
  ; > (sort '(4 23 7 5 16 3) <)
  ; (3 4 5 7 16 23)
  ; > (sort '(4 23 7 5 16 3) >)
  ; (23 16 7 5 4 3)
  ; > (sort '(john paul george ringo) before?)
  ; (GEORGE JOHN PAUL RINGO)
  (check-equal? (sort-19-sent '(4 23 7 5 16 3) <)
                '(3 4 5 7 16 23))
  (check-equal? (sort-19-sent '(4 23 7 5 16 3) >)
                '(23 16 7 5 4 3))
  (check-equal? (sort-19-sent '(john paul george ringo) before?) 
                '(george john paul ringo))
  (check-equal? (sort-19-list '(4 23 7 5 16 3) <)
                '(3 4 5 7 16 23))
  (check-equal? (sort-19-list '(4 23 7 3 5 16 3) <)
                '(3 3 4 5 7 16 23))
  (check-equal? (sort-19-list '(4 23 7 5 4 16 3) <)
                '(3 4 4 5 7 16 23))
  (check-equal? (sort-19-list '(4 23 7 5 16 3) >)
                '(23 16 7 5 4 3))
  (check-equal? (sort-19-list '(john paul george ringo) before?) 
                '(george john paul ringo))

  (check-equal? (remove-once-r 'paul '(john paul george ringo) '())
                '(john george ringo))

  (check-equal? (remove-once-r 'paul '(john paul paul george ringo) '())
                '(john paul george ringo))

  (check-equal? (remove-once-r 'pete '(john paul george ringo) '())
                '(john paul george ringo))

  (check-equal? (remove-once-r 4 '(23 4 7 5 16 3) '())
                '(23 7 5 16 3))
  (check-equal? (remove-once-r 4 '(4 23 4 7 5 16 3) '())
                '(23 4 7 5 16 3))
  (check-equal? (remove-once-r 4 '(23 4 4 7 5 16 3) '())
                '(23 4 7 5 16 3))
  (check-equal? (remove-once-r 4 '(23 4 7 5 16 3 4) '())
                '(23 7 5 16 3 4))
  (check-equal? (remove-once-r 5 '(23 4 7 5 16 3) '())
                '(23 4 7 16 3))


  ;; 19.10
  ;; Mine works about as well as buntine's and mengsince's
  ;; I noticed that structured lists behave like trees (usually)
  ;; but there are many different kind of trees
  (check-equal? (deep-map (lambda (x) (first x)) tiny-world-tree)
                '(w (z (h) (h)) (a (v (m)) ((n s w) (s)) (q (c) ((p d))))))
  (check-equal? (tree-map (lambda (x) (first x)) tiny-world-tree)
                '(w (z (h) (h)) (a (v (m)) (new (s)) (q (c) (port)))))
  (check-equal? (tree-map (lambda (x) (+ 1 x)) num-tree)
                '(4 (5) (8) (3 (4) (9))))
  (check-equal? (deep-map (lambda (x) (+ 1 x)) num-tree)
                '(4 (5) (8) (3 (4) (9))))
  (check-three-things-equal? (tree-map (lambda (x) (+ 1 x)) num-tree)
                             (deep-map (lambda (x) (+ 1 x)) num-tree)
                             '(4 (5) (8) (3 (4) (9))))
  ; neither I nor meng-since can handle this with tree-map
  (check-equal? (deep-map first '(((the man) in ((the) moon)) ate (the) potstickers))
                '(((t m) i ((t) m)) a (t) p))
  ;; (check-equal? )
  ;; 19.11
  ;; using examples from chapter 8
  (check-three-things-equal? '(through the bathroom window)
                             ((repeated bf 3) '(she came in through the bathroom window))
                             (my-repeated bf 3 '(she came in through the bathroom window)))
  (check-three-things-equal? 'computerssss
                             ((repeated more:plural 4)  'computer)
                             (my-repeated more:plural 4 'computer))
  (check-three-things-equal? 81
                             ((repeated more:square 2) 3)
                             (my-repeated more:square 2 3))
  (define (double-sentence sent)
    (se sent sent))
  (check-three-things-equal? '(banana banana banana banana banana banana banana banana)
                             ((repeated double-sentence 3) '(banana))
                             (my-repeated double-sentence 3 '(banana)))
  ;; (check-three-things-equal? )

  ; 19.12
  (check-equal? (tree-reduce         + num-tree) 27)
  (check-equal? (tree-reduce-buntine + num-tree) 27)
  (check-equal? (tree-reduce-meng    + num-tree) 27)
  (check-equal? (tree-reduce-meng append-first-to-list tiny-world-tree) 
                '(world zimbabwe))
  (check-equal? (tree-reduce append-first-to-list tiny-world-tree) 
                '(world zimbabwe))
  (check-equal? (reduce append-first-to-list  (ch17:flatten2 tiny-world-tree)) 
                '(world zimbabwe))
  (check-equal? (my-reduce append-first-to-list '() (ch17:flatten2 tiny-world-tree))
                '(w z h h a v m n s w s q c p d))
  (check-equal? (tree-reduce word-from-first-r tiny-world-tree)
                "w z h h a v m n s w s q c d p")
  ; It looks like meng skips the cities in Zimbabwe for some reason.
  ; If I was using DrRacket, I would insert the emoji for smugness.
  ; They got repeated, but I think I got tree-reduce
  ; That is assuming you flatten properly
  (check-equal? (tree-reduce-meng word-from-first-r tiny-world-tree)
                ; "w z a v m n s w s q c d p"
                "w z a m s q port c")
  (check-equal? (my-reduce word-from-first "" (ch17:flatten2 tiny-world-tree)) 
                " w z h h a v m n s w s q c p d")

  ;; 19.13
  (check-equal? (deep-reduce word '(r ((a (m b) (l)) (e (r)))))
                'rambler)
  (check-equal? (deep-reduce word-from-first-r '(((the man) in ((the) moon)) ate (the) potstickers))
                "t m i t m a p t")
  (check-equal? (deep-reduce + num-tree) 27)
  (check-equal? (deep-reduce word-from-first-r tiny-world-tree)
                "w z h h a v m n s w s q c d p")

) ;; end module+ test 
  
