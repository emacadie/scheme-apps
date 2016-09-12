;; 05 Branching
;; sum of geometric progression
(define (sum-gp a0 r n)
    (* a0
        (if (= r 1)
            n
            (/ (- 1 (expt r n)) (- 1 r)))))

;; exercise 1
;;    A function to return the absolute value of a real number.
;; scheme already has "abs"
(define (tight-abs num)
    (if (>= num 0)
        num
        (- 0 num)))
;; their answer
(define (my-abs n)
  (* n
     (if (positive? n) 1 -1)))
;; oh, my god, becky, these answers are soo elegent. Shido must be, like, one of those engineers or something.
;; did not see the tip to look at functions later in the page

;;    A function to return the reciprocal of a real number. Make it return #f if the argument is 0.
(define (recip num)
    (if (zero? num)
        #f
        (/ num)))
;;    A function to convert a integer to a graphical character of ASCII characters. The integers that can be converted to graphical characters are 33 – 126. Use integer->char to convert a integer to a character. Make it return #f if the given integer can not be converted to a graphical character. 
(define (int->graph-char num)
    (if (and (>= 33 num) (<= 126 num))
        (integer->char num)
        #f))
;; their answer
(define (i2a n)
  (if (<= 33 n 126) ;; I did not know you could do this in Scheme. How cool is that?
      (integer->char n)))

;; "and" and "or"
;; if "and" is true, it returns the last value (evaluates from left to right)
(and #f 0) ;; #f
(and 0 #f) ;; #f
(and 1 2 3) ;; 3
(and 1 2 3 #f) ;; #f
(and 1 2 3 'scheme) ;; scheme

;; also goes left to right, but stops at the first value that evaluates to #t
(or #f 0) ;; 0
(or 0 #f) ;; 0
(or 1 2 3) ;; 1
(or #f 1 2 3) ;; 1
(or #f #f) ;; #f

;; exercise 2
;; A function that takes three real numbers as arguments. It returns the product of these three numbers if all them are positive.
(define (positive-product a b c)
    (if (and (positive? a) (positive? b) (positive? c))
        (* a b c)
        #f))
(positive-product 3 4 5) ;; 60
(positive-product 3 4 -5) ;; #f
(positive-product 3 4 5/3) ;; 20
(positive-product 3 4 -5/3) ;; #f
(positive-product 3 -4 -5) ;; #f

;; A function that takes three real numbers as arguments. It returns the product of these three numbers if at least one of them is negative. 
(define (product-one-neg a b c)
    (if (or (negative? a) (negative? b) (negative? c))
        (* a b c)
        #f))
(product-one-neg 3 4 5) ;; #f
(product-one-neg 3 4 -5) ;; -60
(product-one-neg 3 4 5/3) ;; #f
(product-one-neg 3 4 -5/3) ;; -20
(product-one-neg 3 4 -5/3) ;; -20
(product-one-neg 3 -4 -5/3) ;; 20

;; conditions
(define (fee age)
    (cond
        ((or (<= age 3) (>= age 65)) 0)
        ((<= 4 age 6) 0.5)
        ((<= 7 age 12 ) 1.0)
        ((<= 13 age 15) 1.5)
        ((<= 16 age 18) 1.8)
        (else 2.0)))

;; exercise 3
;; The grade (A – D) of exam is determined by the score (score). Write a function that takes a score as an argument and returns a grade.
;; A if score ≥ 80
;; B if 60 ≤ score ≤ 79
;; C if 40 ≤ score ≤ 59
;; D if score < 40 
(define (grade-exam score)
    (cond
        ((>= score 80) 'A)
        ((<= 60 score 79) 'B)
        ((<= 40 score 59) 'C)
        (else 'D)))

;; functions that make predicates
;; eq? compares addresses of two args
(define str "hello")
(eq? str str) ;; #t 
(eq? "hello" "hello") ;; #t ;; #f in chex and guile
(eq? 1 1) ;; #t
(eq? "jj" "jj") ;; #t
(eq? 1.0 1.0) ;; #f

;; eqv? compares types and values of two objects
(eqv? 1.0 1.0) ;; #t
(eqv? 1 1.0) ;; #f
(eqv? '(1 2 3) '(1 2 3)) ; #f
(eqv? "hello" "hello") ;; #t, #f in chez
(eqv? (lambda (x) x) (lambda (x) x)) ;; #f

;; equal? compares sequences like lists or strings
(equal? '(1 2 3) '(1 2 3)) ;; #t
(equal? '(1 2 3) '(1 2 4)) ;; #f
(equal? "hello" "hello") ;; #t

;; comparing numbers



