#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         ; (prefix-in rd:   racket/date)
         (prefix-in srfi-19: srfi/19))

(provide a-friend     ; chapter 08
         a-pair?      ; chapter07
         addtup       ; chapter 04
         all-nums     ; chapter 04
         atom?        ; preface
         display-all  ; added by me
         display-date ; added by me
         edd1         ; chapter 06
         eqan?        ; chapter 04
         eq?-c        ; chapter 08
         eq?-salad    ; chapter 08
         eq?-tuna     ; chapter 08
         eqlist?      ; chapter 05
         eqlist2?     ; chapter 05
         eqlist5?     ; chapter 05
         eqset?       ; chapter 07
         equal2?      ; chapter 05
         equal5?      ; chapter 05

         firsts        ; chapter 03
         fullfun?      ; chapter 07
         fun?          ; chapter 07
         insertL       ; chapter 03
         insertL-f     ; chapter 08
         insertL2      ; chapter 08
         insertR       ; chapter 03
         insertR*      ; chapter 05
         insertR-f     ; chapter 08
         intersect     ; chapter 07
         intersect?    ; chapter 07
         intersectall  ; chapter 07
         last-friend   ; chapter 08
         lat?          ; chapter 02
         leftmost      ; chapter 05
         makeset       ; chapter 07
         member?       ; chapter 02
         member*       ; chapter 05
         multiinsertL  ; chapter 03
         multiinsertR  ; chapter 03
         multirember   ; chapter 03
         multirember&co ; chapter 08
         multirember-eq? ; chapter 08
         multirember-f ; chapter 08
         multiremberT  ; chapter 08
         multisubst    ; chapter 03
         my+           ; chapter 04
         my-           ; chapter 04
         my-add1       ; chapter 04
         my-div        ; chatper 04
         my-eq         ; chapter 04
         my-gt         ; chapter 04
         my-length     ; chapter 04
         my-lt         ; chapter 04
         my-rember     ; I may get rid of this
         my-sub1       ; chapter 04
         my-x          ; chapter 04
         no-nums       ; chapter 04
         numbered?     ; chapter 06
         numbered2?    ; chapter 06
         ny+           ; chapter 06
         occur         ; chapter 04
         occur*        ; chapter 05
         one?          ; chapter 04
         one-to-one?   ; chapter 07
         pick          ; chapter 04
         raise-power   ; chapter 04
         rember        ; chapter 03
         rember-eq?    ; chapter 08
         rember-f      ; chapter 08
         rember-f2     ; chapter 08
         rember8       ; chapter 08
         rember*       ; chapter 05
         rempick       ; chapter 04
         revrel        ; chapter 07
         seconds       ; chapter 07
         sero?         ; chapter 06
         set?          ; chapter 07
         subset?       ; chapter 07
         subst         ; chapter 03
         subst*        ; chapter 05
         subst2        ; chapter 03
         subst8        ; chapter 08
         tup+          ; chapter 04
         union         ; chapter 07
         value         ; chapter 06
         value2        ; chapter 06
         value8        ; chapter 08
         zub1          ; chapter 06
)

#|
Stats:
chapter 02: 2
chapter 04: 20
|#

; in preface
(define (atom? x)
  (rb6:and (rb6:not (pair? x))
           (rb6:not (null? x))))

(define (display-all . vs)
  (for-each ris6:display vs)
  (ris6:newline))

(define (display-date)
  (let ([the-date (seconds->date (current-seconds))] )
    (srfi-19:date->string the-date "~Y-~m-~d ~H:~M:~S")))

; defined in chapter 2, after it is used
; lat means list of atoms, I think
; I think they just use it to see if a list is flat.
; It chokes if you give it an atom.
(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l)) (lat? (cdr l))]
        [else #f]))

; Mine is tail-recursive.
(define (member? the-atom the-list)
  (cond [(null? the-list) #f]
        [(equal5? the-atom (car the-list)) #t]
        [else (member? the-atom (cdr the-list))]))

; Chapter 03
(define (rember-helper a lat out-lat)
  (display-all "rember-helper called with " a ", " lat ", " out-lat)
  (cond [(null? lat) out-lat]
        [(eq? a (car lat)) (cons out-lat (cdr lat))]
        [else (rember-helper a (cdr lat) (cons (car lat) out-lat))]))

(define (my-rember a lat)
  (rember-helper a lat '()))

(define (rember a lat)
  (cond [(null? lat) '()]
        ; [(eq? (car lat) a) (cdr lat)]
        [(equal5? (car lat) a) (cdr lat)] ; from chapter 5
        [else (cons (car lat) (rember a (cdr lat)))])) 

(define (firsts l)
  (cond [(null? l) '()]
        [else (cons (car (car l)) (firsts (cdr l)))]))

(define (insertR new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) (cons old (cons new  (cdr lat)))]
        [else (cons (car lat) (insertR new old (cdr lat)))]))
; Yo dawg, I heard you like cons-ing lists, so I put a cons in your cons
; so you can add a list to your list.
; I really hate the way they put another "cond" inside their "else" clause.

(define (insertL new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) (cons new lat)]
        [else (cons (car lat) (insertL new old (cdr lat)))]))

; replace old with new
(define (subst new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) (cons new (cdr lat))]
        [else (cons (car lat) (subst new old (cdr lat)))]))

; replace either o1 or o2 with new
; now we get "or"
(define (subst2 new o1 o2 lat)
  (cond [(null? lat) '()]
        [(or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))]
        [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))]))

(define (multirember a lat)
  (cond [(null? lat) (quote ())]
        [(equal5? (car lat) a) (multirember a (cdr lat))]
        [else (cons (car lat) (multirember a (cdr lat)))])) 

(define (multiinsertR new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) 
         (cons old (cons new (multiinsertR new old (cdr lat))))]
        [else (cons (car lat) (multiinsertR new old (cdr lat)))]))

(define (multiinsertL new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) 
         (cons new (cons old (multiinsertL new old (cdr lat))))]
        [else (cons (car lat) (multiinsertL new old (cdr lat)))]))

(define (multisubst new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) (multisubst new old (cons new (cdr lat)))]
        [else (cons (car lat) (multisubst new old (cdr lat)))]))

; Chapter 04
;; racket/base has a function "add1", but I will "add one" of my own.
(define (my-add1 x)
  (+ 1 x))

; sub1 is another that is in racket/base, but not R6RS.
; I assume they want us to write these ourselves.
; Should I check for positivity?
(define (my-sub1 x)
  (- x 1))

; define "+" (prefix w/"my" to distinguish from built-in "+")
; recurse using add1
(define (my+ x y)
  (cond [(rb6:zero? y) x]
        [else (my+ (add1 x) (sub1 y))]))

(define (my- x y)
  (cond [(rb6:zero? y) x]
        [else (my- (sub1 x) (sub1 y))]))

(define (addtup tup)
  (cond [(null? tup) 0]
        [else (my+ (car tup) (addtup (cdr tup)))]))

(define (my-x n m)
  (cond [(rb6:zero? m) 0]
        [else (my+ n (my-x n (sub1 m)))]))

(define (tup+ tup1 tup2)
  (cond [(null? tup1) tup2]
        [(null? tup2) tup1]
        [else (cons (my+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]))

; using "gt" for greater than instead of ">" 
; since that can be an arrow for conversion, like "string->number"
(define (my-gt x y)
  (cond [(rb6:zero? x) #f]
        [(rb6:zero? y) #t]
        [else (my-gt (sub1 x) (sub1 y))]))

; less than
(define (my-lt x y)
  (cond [(rb6:zero? y) #f]
        [(rb6:zero? x) #t]        
        [else (my-lt (sub1 x) (sub1 y))]))

; equal, for numbers. eq? is for other atoms
(define (my-eq x y)
  (cond [(my-lt x y) #f]
        [(my-gt x y) #f]
        [else #t]))

; use the first and fifth commandments
; Raise a number to a power; I cannot use the arrow symbol they have
(define (raise-power num pow)
  (cond [(zero? pow) 1]
        [else (my-x num (raise-power num (sub1 pow)))]))

; quotient only
(define (my-div n m)
  (cond [(my-lt n m) 0]
        [else (add1 (my-div (my- n m) m))]))

(define (my-length lat)
  (cond [(null? lat) 0]
        [else (add1 (my-length (cdr lat)))]))

(define (pick n lat)
(cond [(null? lat) '()]
      [(eq? n 1) (car lat)]
      [else (pick (sub1 n) (cdr lat))]))

; rempick was re-written; see below.

(define (no-nums lat)
  (cond [(null? lat) '()]
        [(number? (car lat)) (no-nums (cdr lat))]
        [else (cons (car lat) (no-nums (cdr lat)))]))

(define (all-nums lat)
  (cond [(null? lat) '()]
        [(number? (car lat)) (cons (car lat) (all-nums (cdr lat))) ]
        [else (all-nums (cdr lat))]))

; I assume we were supposed to use the "=" function we made in chapter 04
(define (eqan? a1 a2)
  (cond [(rb6:and (number? a1) (number? a2) (my-eq a1 a2)) #t]
        [(rb6:and (not (number? a1)) (not (number? a2)) (eq? a1 a2)) #t]
        [else #f]))

; count number of times atom a appears in lat
(define (occur a lat)
  (cond [(null? lat) 0]
        [(eqan? a (car lat)) (add1 (occur a (cdr lat)))]
        [else (occur a (cdr lat))]))

(define (one? n)
  (eqan? n 1))

(define (rempick n lat)
  (cond [(one? n) (cdr lat)]
        [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])) 

; chapter 05
; my attempt
; I admit, I had to incorporate some of theirs.
; And I changed it a bit, since I hate nested conds
; I admit, I did not notice they were actually doing 
; a nested multirember, not a nested rember
(define (rember* a l)
  (cond [(null? l) '()]
        ; this is the new part
        [(not (atom? (car l))) (cons (rember* a (car l)) (rember* a (cdr l)))]
        ; this is like multirember, adding check for atom? before equality check
        [(rb6:and (atom? (car l)) (eqan? (car l) a)) (rember* a (cdr l))]
        ; this is also like multirember
        [else (cons (car l) (rember* a (cdr l)))])) 

(define (insertR* new old lat)
  (cond [(null? lat) '()]
        [(not (atom? (car lat))) (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))]
        [(rb6:and (atom? (car lat)) (eq? (car lat) old)) 
         (cons old (cons new (insertR* new old (cdr lat))))]
        [else (cons (car lat) (insertR* new old (cdr lat)))]))

; count number of times atom a appears in lat
(define (occur* a lat)
  (cond [(null? lat) 0]
        [(not (atom? (car lat))) (my+ (occur* a (car lat)) (occur* a (cdr lat)))]
        [(rb6:and (atom? (car lat)) (eqan? a (car lat))) (add1 (occur* a (cdr lat)))]
        [else (occur* a (cdr lat))]))
; For these, their "else" is my "not atom?" part.
(define (subst* new old lat)
  (cond [(null? lat) '()]
        [(not (atom? (car lat))) (cons (subst* new old (car lat)) (subst* new old (cdr lat)))]
        [(rb6:and (atom? (car lat)) (eqan? (car lat) old)) (subst* new old (cons new (cdr lat)))]
        [else (cons (car lat) (subst* new old (cdr lat)))]))

(define (insertL* new old lat)
  (cond [(null? lat) '()]
        [(not (atom? (car lat))) (cons (insertL* new old (car lat)) (insertR* new old (cdr lat)))]
        [(rb6:and (atom? (car lat)) (eqan? (car lat) old)) 
         (cons new (cons old (insertL* new old (cdr lat))))]
        [else (cons (car lat) (insertL* new old (cdr lat)))]))
#|
Mine:
(define (member* the-atom the-list)
  (cond [(null? the-list) #f]
        [(not (atom? (car the-list))) (or (member* the-atom (car the-list))
                                          (member* the-atom (cdr the-list)))]
        [(rb6:and (atom? (car the-list)) (eqan? the-atom (car the-list))) #t]
        ;[(rb6:and (atom? (car the-list)) (not (eqan? the-atom (car the-list)))) #f]
        [else #f]))
|#
; Theirs is better:
(define (member* a l)
  (cond [(null? l) #f]
        ; I had "(member* a (cdr l))" in my else for a while. I was pretty close.
        [(atom? (car l)) (or (eqan? (car l) a) (member* a (cdr l)))]
        ; I got the else right
        [else (or (member* a (car l)) (member* a (cdr l)))])) 

(define (leftmost l)
  (cond [(null? l) '()]
        [(atom? (car l)) (car l)]
        [else (leftmost (car l))]))

(define (eqlist? l1 l2)
  (cond [(rb6:and (null? l1) (null? l2)) #t]
        [(rb6:and (not (atom? (car l1))) 
                  (not (atom? (car l2)))) 
           (rb6:and (eqlist? (car l1) (car l2))
                    (eqlist? (cdr l1) (cdr l2)))]
        [(rb6:and (atom? (car l1)) 
                  (atom? (car l2))
                  (eqan? (car l1) (car l2))) 
         (eqlist? (cdr l1) (cdr l2))]
        ; [(rb6:and (atom? (car l1)) (atom? (car l2)))]
        [else #f]))

; their version l 346
(define (eqlist2? l1 l2)
  (cond [(and (null? l1) (null? l2)) #t]
        [(and (null? l1) (atom? (car l2))) #f]
        [(null? l1 ) #f]
        [(and (atom? (car l1)) (null? l2)) #f]
        [(and (atom? (car l1)) (atom? (car l2)))
         (and (eqan? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2)))]
        [(atom? (car l1)) #f]
        [(null? l2) #f]
        [(atom? (car l2)) #f]
        [else (and (eqlist2? (car l1) (car l2))
                   (eqlist2? (cdr l1) (cdr l2)))])) 

; their second version
(define (eqlist3? l1 l2)
  (cond [(and (null? l1) (null? l2)) #t]
        [(or (null? l1) (null? l2)) #f]
        [(and (atom? (car l1)) (atom? (car l2)))
         (and (eqan? (car l1) (car l2))
              (eqlist3? (cdr l1) (cdr l2)))]
        [(or (atom? (car l1)) (atom? (car l2))) #f]
        [else (and (eqlist3? (car l1) (car l2))
                   (eqlist3? (cdr l1) (cdr l2)))]))

; calling this equal2? since "equal?" is already in Racket and R6RS
#|
; my first version
; works, but after seeing theirs I see a few chances for improvement
(define (equal2? a b)
  (cond [(rb6:and (null? a) (null? b)) #t]
        [(rb6:and (atom? a) (atom? b) (eqan? a b)) #t]
        [(rb6:and (not (atom? a)) (not (atom? b))) 
        (rb6:and (equal2? (car a) (car b)) (equal2? (cdr a) (cdr b)))]
        [else #f]))
|#
(define (equal2? a b)
  (cond [(rb6:and (atom? a) (atom? b)) (eqan? a b)]
        [else (eqlist? a b)]))

; their first version
(define (equal3? s1 s2)
  (cond [(rb6:and (atom? s1) (atom? s2)) (eqan? s1 s2)]
        [(atom? s1) #f] ; we know s2 is not an atom if this is true
        [(atom? s2) #f] ; If you get here, you know s1 is NOT an atom
        [else (eqlist? s1 s2)]))

; their second version
(define (equal4? s1 s2)
  (cond [(rb6:and (atom? s1) (atom? s2)) (eqan? s1 s2)]
        [(or (atom? s1) (atom? s2)) #f] 
        [else (eqlist? s1 s2)]))
; now rewrite eqlist? using equal2?
; using their second version

; to do the mutual calls, I need their equal, not mine.
(define (equal5? a b)
  (cond [(rb6:and (atom? a) (atom? b)) (eqan? a b)]
        [(rb6:or (atom? a) (atom? b)) #f] 
        [else (eqlist5? a b)]))

; no eqlist4?
(define (eqlist5? l1 l2)
  (cond [(rb6:and (null? l1) (null? l2)) #t]
        [(rb6:or (null? l1) (null? l2)) #f]
        [else (rb6:and (equal5? (car l1) (car l2))
                       (eqlist5? (cdr l1) (cdr l2)))]))

; chapter 6
#|
(define (numbered? x)
  (display-all "In numbered? with x: " x)
  (cond [(null? x) #t]
        [(and (atom? x) (number? x)) #t]
        [(not (atom? x)) (and (numbered? (car x)) (numbered? (cdr x)))]
        [(number? (car x)) (numbered? (cdr x))]
        [(or (eqan? (car x) '+) 
             (eqan? (car x) '*) 
             (eqan? (car x) 'raise-power)) (numbered? (cdr x))]
        [else #f]
)
)
|#
; theirs
; I don't think I would have gotten this -
; I hate car-ring and cdr-ing like this.
#|
(define (numbered? aexp) 
  (cond [(atom? aexp) (number? aexp)]
        [(eq? (car (cdr aexp)) '+)
         (and (numbered? (car aexp))
              (numbered? (car (cdr (cdr aexp)))))]
        [(eq? (car (cdr aexp)) '*)
         (and (numbered? (car aexp))
              (numbered? (car (cdr (cdr aexp)))))]
        [(eq? (car (cdr aexp)) 'raise-power)
         (and (numbered? (car aexp))
              (numbered? (car (cdr (cdr aexp)))))]))
|#
; simplified
(define (numbered? aexp) 
  (cond [(atom? aexp) (number? aexp)]
        [else (rb6:and (numbered? (car aexp))
                       (numbered? (car (cdr (cdr aexp)))))]))

(define (value aexp)
  (cond [(not (numbered? aexp)) '()]
        [(atom? aexp) aexp]
        [(eq? (car (cdr aexp)) '+)
         (+ (value (car aexp)) (value (car (cdr (cdr aexp)))))]
        [(eq? (car (cdr aexp)) '*)
         (* (value (car aexp)) (value (car (cdr (cdr aexp)))))]
        [(eq? (car (cdr aexp)) 'raise-power)
         (raise-power (value (car aexp)) (value (car (cdr (cdr aexp)))))]))

; I still want to check if the new lists are proper arithmatic expressions
; more verbose than I would like, but it works
(define (numbered2? aexp)
  (cond [(null? aexp) #t] 
        [(atom? aexp) (number? aexp)]
        [(eqan? (car aexp) '+) (numbered2? (cdr aexp))]
        [(eqan? (car aexp) '*) (numbered2? (cdr aexp))]
        [(eqan? (car aexp) 'raise-power) (numbered2? (cdr aexp))]
        [(number? (car aexp)) (numbered2? (cdr aexp))]
        [(not (lat? aexp)) (rb6:and (numbered2? (car aexp))
                                    (numbered2? (cdr aexp)))]
        [else #f]))

(define (1st-sub-exp aexp)
  (cond [else (car (cdr aexp))]))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

; verbose, but more meaningful
(define (operator aexp)
  (car aexp))

(define (value2 nexp)
  (cond [(atom? nexp) nexp]
        [(eq? (operator nexp) '+) (my+ (value2 (1st-sub-exp nexp))
                                       (value2 (2nd-sub-exp nexp)))]
        [(eq? (operator nexp) '*) (my-x (value2 (1st-sub-exp nexp))
                                        (value2 (2nd-sub-exp nexp)))]
        [else (raise-power (value2 (1st-sub-exp nexp))
                           (value2 (2nd-sub-exp nexp)))]))

(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

(define (ny+ n m)
  (cond [(sero? m) m]
        [else (ny+ (edd1 n) (zub1 m))]))

; chapter 07
(define (set? lat)
  (cond [(null? lat) #t]
        [(member? (car lat) (cdr lat)) #f]
        [else (set? (cdr lat))]))

(define (makeset lat)
  (cond [(null? lat) '()]
        [else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))]))

; checks if each atom in s1 is in s2
; NOT if all of s1 is in s2 in current order
(define (subset? s1 s2)
  (cond [(null? s1) #t]
        [(member? (car s1) s2) (subset? (cdr s1) s2)]
        [else #f]))
; You could change the else to use "and", 
; with the functions on the second line at the args
#|
(define (eqset? s1 s2)
  (cond [(null? s1) #t]
        [(rb6:and (not (set? s1)) (not (set? s2))) #f]
        [(member? (car s1) s2) (eqset? (cdr s1) s2)]
        [else #f]))
|#
; They did subset? twice, checking each against the other
(define (eqset? s1 s2)
  (rb6:and (subset? s1 s2)
           (subset? s2 s1)))

; Mine is shorter, seems to work
(define (intersect? s1 s2)
  (cond [(null? s1) #f]
        [(not (member? (car s1) s2)) (intersect? (cdr s1) s2)]
        [else #t]))

(define (intersect s1 s2)
  (cond [(null? s1) '()]
        [(member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2))]
        [else (intersect (cdr s1) s2)]))

(define (union s1 s2)
  (cond [(null? s1) s2]
        [(member? (car s1) s2) (union (cdr s1) s2)]
        [else (cons (car s1) (union (cdr s1) s2))]))

(define (set-diff set1 set2)
  (cond [(null? set1) '()]
        [(member? (car set1) set2) (set-diff (cdr set1) set2)]
        [else (cons (car set1) (set-diff (cdr set1) set2))]))
#|
(define (intersectall lset)
  (display-all "intersectall with lset: " lset)
  (cond [(and (not (null? (car lset)))
              (not (null? (cdr lset)))) 
         (begin
           (display-all "(car lset): " (car lset) ", (car (cdr lset)): "
                        (car (cdr lset)))
           (cons (intersect (car lset) (car (cdr lset))) (intersectall (cdr lset)))
)]
        [else '()]))
|#
; I should have seen this.
(define (intersectall l-set)
  (cond [(null? (cdr l-set)) (car l-set)]
        [else (intersect (car l-set) (intersectall (cdr l-set)))])) 

#|
; mine is shorter, and it works, but I will go with theirs.
(define (a-pair? l)
  (cond [(eqan? (length l) 2) #t]
        [else #f]))
|#

(define (a-pair? x)
  (cond [(atom? x) #f]
        [(null? x) #f]
        [(null? (cdr x)) #f]
        [(null? (cdr (cdr x))) #t]
        [else #f])) 

(define (first x) 
  (car x))
(define (second x)
  (car (cdr x)))
(define (build s1 s2)
  (cons s1 (cons s2 '())))
(define (third x)
  (car (cdr (cdr x))))

(define (fun? x)
  (set? (firsts x)))

(define (revpair pair)
  (build (second pair) (first pair)))

(define (revrel x)
  (cond [(null? x) '()]
        [else (cons (revpair (car x)) (revrel (cdr x)))]))

(define (seconds l)
  (cond [(null? l) '()]
        [else (cons (second (car l)) (seconds (cdr l)))]))

(define (fullfun? fun)
  (set? (seconds fun))) 

; This does the same thing:
(define (one-to-one? fun)
  (fun? (revrel fun))) 

; chapter 08
(define (rember-f the-func a lat)
  (cond [(null? lat) '()]
        ; [(eq? (car lat) a) (cdr lat)]
        [(the-func (car lat) a) (cdr lat)] ; from chapter 5
        [else (cons (car lat) (rember-f the-func a (cdr lat)))]))
; according to book, in Lisp, you would do this for function call:
; (funcall the-func (car lat) a)

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(define rember-f2
  (lambda (test?)
    (lambda (a lat)
      (cond [(null? lat) '()]
            [(test? (car lat) a) (cdr lat)]
            [else (cons (car lat) ((rember-f2 test?) a (cdr lat)))]))))
; Honestly, not too clear why you would want a lambda inside a lambda.
; Maybe after "Simply Scheme" it should be obvious, but I don't get it.  
; WRT currying: I can see using lambdas in higher-order functions,
; esp the Big Three. But then just use a straight lambda.

(define rember-eq? (rember-f2 eq?))

(define insertL-f 
  (lambda (test?)
    (lambda (new old lat)
      (cond [(null? lat) '()]
            [(test? (car lat) old) (cons new lat)]
            [else (cons (car lat) ((insertL-f test?) new old (cdr lat)))]))))


#|
; This is based on their first version
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond [(null? l) (quote ())]
            [(test? (car l) old) (cons new (cons old (cdr l)))]
            [else (cons (car l) ((insertL-f test?) new old (cdr l)))]))))
|#

(define insertR-f 
  (lambda (test?)
    (lambda (new old lat)
      (cond [(null? lat) '()]
        [(test? (car lat) old) (cons old (cons new (cdr lat)))]
        [else (cons (car lat) ((insertR-f test?) new old (cdr lat)))]))))

; If we look at their insertL-f, all of it but one line are pretty much the same
; Will they do it with a cond? Or with another function?

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))
; with functions

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond [(null? l) (quote ())]
            ; calling passed seq function
            [(eq? (car l) old) (seq new old (cdr l))] 
            [else (cons (car l) ((insert-g seq) new old (cdr l)))]))))

(define insertL2
  (insert-g (lambda (new old l)
              (cons new (cons old l)))))
; define function in-line

; subst is a lot like insertR and insertL
; let's make a seq for that
(define (seqS new old l)
  (cons new l))
; we need "old" since we will put this into insert-g for a new subst

(define subst8
  (insert-g seqS))
; can I call this with args?

(define seqrem
  (lambda (new old l)
    l))

(define (rember8 a l)
  ((insert-g seqrem) #f a l))
; #f is a placeholder, I think.

; this returns functions
(define (atom-to-function x)
  (cond [(eqan? x '+) my+]
        [(eqan? x '*) my-x]
        [else raise-power])) 
; this expects expressions like '(+ 1 2), not '(1 + 2)
(define (value8 nexp)
  (display-all "Calling value8 with " nexp)
  (cond [(atom? nexp) nexp]
        [else 
         ; the double parens were tricky
         ((atom-to-function (operator nexp)) (value8 (1st-sub-exp nexp))
                                             (value8 (2nd-sub-exp nexp)))]))
; I am having some trouble thinking functionally.
; Perhaps I should watch Eric Normand's podcast more often.

(define (multirember-f test)
  (lambda (a lat) 
  (cond [(null? lat) (quote ())]
        [(test (car lat) a) ((multirember-f test) a (cdr lat))]
        [else (cons (car lat) ((multirember-f test) a (cdr lat)))])))

#|
(define multirember-f
  (lambda (test ?)
    (lambda (a lat)
      (cond ((null? lat) (quote ()))
            ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))
|#

; define Define multirember-eq ? using multirember-f
; I forgot: No need to specify "a" and "lat"
(define (multirember-eq? test)
    ((multirember-f eq?)))

(define eq?-tuna (eq?-c 'tuna))

#|
Perhaps we should now write multiremberT which is similar to multirember-f
Instead of taking testP and returning a function, 
multiremberT takes a function like eq?-tuna and a lat and then does its work.
|#
(define (multiremberT the-func lat)
  (cond [(null? lat) '()]
        [(the-func (car lat)) (multiremberT the-func (cdr lat))]
        [else (cons (car lat) (multiremberT the-func (cdr lat)))]))
; Let me guess: Will we use "lambda" in an invocation of multiremberT
; instead of pre-defining ahead-of-time?

; Their function from page 137
(define multirember&co
  (lambda (a lat col)
    (cond [(null? lat) (col (quote ()) (quote ()))]
          [(eq? (car lat) a)
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col newlat
                                  (cons (car lat) seen))))]
          [else
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col (cons (car lat) newlat)
                                  seen)))])))

; It is a function that takes two arguments 
; and asks whether the second one is the empty list. 
; It ignores its first argnment.
(define a-friend
  (lambda (x y)
    (null? y)))

; multirember&co is not really removing, it is returning #t or #f
; at least with "a-friend"
; "col" stands for "collector", which is also a continuation.

; from the text, page 140, about what (multirember&co a lat f) does:
; It looks at every atom of the lat to see whether it is eq? to a. 
; Those atoms that are not are collected in one list ls1 
; the others for which the answer is true are collected in a second list ls2 . 
; Finally, it determines the value of (f ls1 ls2 ) .

(define (last-friend x y)
  (length x))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (runit:check-equal? (rb6:rational-valued? 6/10) #t)
  (display-all "testing " "testing")
  (runit:check-equal? (atom? (quote ())) #f)
  (runit:check-equal? (atom? (rb6:quote ())) #f)
  (runit:check-equal? (atom? '()) #f)

  (runit:check-equal? (operator '(+ 5 3))
                      '+)

)

  


