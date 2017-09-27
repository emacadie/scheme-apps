;; Higher-order functions
;; every is like map
;; "every transforms each element of a word or sentence individually. The result sentence usually contains as many elements as the argument"

;; keep is like filter
;; "keep selects certain elements of a word or sentence and discards the others. 
;; The elements of the result are elements of the argument, without transformation, 
;; but the result may be smaller than the original. 

;; accumulate is like reduce
;; "accumulate transforms the entire word or sentence into a single result by combining all of the elements in some way. ""

;; another way of thinking about it:
;; every      -> transform
;; keep       -> select
;; accumulate -> combine

(define (add-numbers sent)
  (accumulate + (keep number? sent)))
 
(add-numbers '(4 calling birds 3 french hens 2 turtle doves))

 
(add-numbers '(1 for the money 2 for the show 3 to get ready
		   and 4 to go))

(define (always-one arg)
  1)

(define (count sent)
  (accumulate + (every always-one sent)))

(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (acronym phrase)
  (accumulate word (every first (keep real-word? phrase))))

;; ((repeated plural 4) 'computer)
;; ((repeated #function #number-of-times) args-to-function)

