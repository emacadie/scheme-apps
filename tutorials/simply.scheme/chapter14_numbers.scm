;; The project after chapter 14
;; Write a procedure number-name that takes a positive integer argument and returns a sentence containing that number spelled out in words:

;; > (number-name 5513345)
;; (FIVE MILLION FIVE HUNDRED THIRTEEN THOUSAND THREE HUNDRED FORTY FIVE)

;; > (number-name (factorial 20))
;; TWO QUINTILLION FOUR HUNDRED THIRTY TWO QUADRILLION NINE HUNDRED TWO
;; TRILLION EIGHT BILLION ONE HUNDRED SEVENTY SIX MILLION SIX HUNDRED
;; FORTY THOUSAND

;; There are some special cases you will need to consider:
;; - Numbers in which some particular digit is zero
;; - Numbers like 1,000,529 in which an entire group of three digits is zero.
;; - Numbers in the teens. 
;; This is nothing but special cases
;; I assume the idea is to use recursion


(define (exponent-helper the-ex the-num)
  (display-all "calling exponent-helper with the-ex: " the-ex ", the-num: " the-num)
  (cond ((equal? the-ex 0) the-num)
        ((equal? the-ex 2) 'hundred)
        ((equal? the-ex 3) 'thousand)
        ((equal? the-ex 6) '(million))
        ((equal? the-ex 9) '(billion))
        ((equal? the-ex 12) '(trillion))))

(define number-table '(one two three four five six seven eight nine zero))

(define teen-table '(eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen))

(define (get-teen-table the-num)
  (if (equal? the-num 0) 
      'ten
      (item the-num teen-table)))

(define power-1-table '(ten twenty thirty forty fifty sixty seventy eighty ninety))

(define (power-1-helper the-num)
  (item the-num power-1-table))

(define (get-last-3 the-num)
  (word (last (butlast (butlast the-num)))
        (last (butlast the-num))
        (last the-num)))

;; the-exp is an exponent, call with 0 
(define (number-name-r the-num the-exp outp depth)
  (display-all "calling number-name-r with the-num: " the-num ", the-exp: " the-exp ", outp: " outp ", depth: " depth)
  (cond ((empty? the-num) outp)
        
        ((and (equal? the-exp 0) (>= (count the-num) 2) (equal? (last (butlast the-num)) 1)) 
         (display-all "exponent = 0, at least 2 chars in num, 2nd to last = 1") ;; handles teens
         (number-name-r (butlast (butlast the-num)) 
                        (+ 2 the-exp) 
                        (sentence (get-teen-table (last the-num)) outp)
                        0))
        ;; do saul and david here (thousands and tens of thousands)
        ;; for exp of anything divisible by 3, treat it like something less that 1000
        ;; and tack "thousand" or "million" on the end as needed
        ;; So this is really "yo-dawg": a recursive call in your recursive call
        ((and (member? the-exp '(3 6 9 12)) (equal? (count the-num) 1)) 
         (display-all "dealing with thousands")
         (number-name-r (butlast the-num) 
                        (+ 1 the-exp) 
                        (sentence (item (first the-num) number-table) 
                                  (exponent-helper the-exp 0) 
                                  outp)
                        0))
        ((and (member? the-exp '(3 6 9 12)) (equal? (count the-num) 2))
         (display-all "dealing with tens of thousands")
         (number-name-r (butlast (butlast the-num)) 
                        (+ 2 the-exp) 
                        (sentence (number-name-r the-num 
                                                 0 
                                                 '()
                                                 0) 
                                  (exponent-helper the-exp 0) 
                                  outp)
                        0))
        ((and (member? the-exp '(3 6 9 12)) (>= (count the-num) 3)) ; (equal? (count the-num) 3)
         (display-all "dealing with hundreds of thousands")
         (number-name-r (butlast (butlast (butlast the-num))) 
                        (+ 3 the-exp) 
                        (sentence (number-name-r (get-last-3 (trim-leading-zeros the-num)) 
                                                 0 
                                                 '()
                                                 0) 
                                  (exponent-helper the-exp 0) 
                                  outp)
                        0))

       ((equal? (last the-num) 0)
         (display-all "the last part of num is 0")
         (number-name-r (butlast the-num) 
                        (+ 1 the-exp) 
                        outp
                        0))
        ((equal? the-exp 0)
         (display-all "The exp is 0")
         (number-name-r (butlast the-num) 
                        (+ 1 the-exp) 
                        (sentence (item (last the-num) number-table) outp)
                        0))
               
        ((member? the-exp '(1)) 
         (display-all "the exponent is a member of '(1)")
         (number-name-r (butlast the-num) 
                        (+ 1 the-exp) 
                        (sentence (item (last the-num) power-1-table) outp)
                        0))
        (else
         (display-all "in the else")
         (number-name-r (butlast the-num) 
                        (+ 1 the-exp) 
                        (sentence (item (last the-num) number-table) (exponent-helper the-exp (last the-num)) outp)
                        0))))

(define (number-name the-num)
  (number-name-r (trim-leading-zeros (round (abs the-num))) 
                 0 
                 '()
                 0))

;; Ugly, but done
;; I will try to take out the "depth" later
;; line 136
