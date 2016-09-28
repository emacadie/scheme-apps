;; 07 repetition
;; recursion
(define (fact n)
    (if (= n 1)
        1
        (* n (fact (- n 1)))))

(fact 3) ;; 6
(fact 4) ;; 24

(define (list*2 ls)
    (if (null? ls)
        '()
        (cons (* 2 (car ls))
            (list*2 (cdr ls)))))
(list*2 '(1 2 3)) ;; (2 4 6)

;; exercise 1
;; These are kind of hard. With Clojure, I would make functions with multiple arities, but I don't know if Scheme can do that.
;; I think if it were possible, shido would have mentioned it.
;; A function that counts the number of list items, my-length. (Function length is pre-defined.)
(define (my-length ls)
    (if (null? ls)
        0
        (+ 1 (my-length (cdr ls)))))

(my-length '(9 8 7)) ;; 3
(my-length '('Scheme 'is 'really 'amazing)) ;; 4
;; shido's answer
(define (my-length ls)
  (if (null? ls)
      0
      (+ 1 (my-length (cdr ls)))))

;; A function that summarizes numbers in a list.
(define (sum-num ls)
    (if (null? ls)
        0
        (+ (car ls) (sum-num (cdr ls)))))
(sum-num '(1 2 3)) ;; 6
(sum-num '(500 800 9)) ;; 1309
;; shido's answer
(define (my-sum ls)
  (if (null? ls)
      0
      (+ (car ls) (my-sum (cdr ls)))))

;; A function that takes a list (ls) and an object (x) as arguments and returns a list removing x from ls.
(define (remove-item ls x)
    (if (= x (car ls))
        (cdr ls)
        (cons (car ls) (remove-item (cdr ls) x))))
(remove-item '(1 2 3 4) 3) ;; (1 2 4)
(remove-item '(1 2 3 4 5 3) 3) ;; this returns (1 2 4 5 3)
;; shido's answer 
(define (remove x ls)
  (if (null? ls)
      '()
      (let ((h (car ls)))
        ((if (eqv? x h)
            (lambda (y) y)
            (lambda (y) (cons h y)))
         (remove x (cdr ls))))))

;; A function that takes a list (ls) and an object (x) and returns the first position of x in ls. The position is counted from 0. If x is not found in ls, the function returns #f. 
;; this one was hard, did not finish
(define (find-first-instance ls x)
    (cond ((= (length ls) 0) #f) ;;  ((null? ls) #f)
        ((= x (car ls)) 0)
        (else (+ 1 (find-first-instance (cdr ls) x)))))
(find-first-instance '(1 2 3) 1) ;; 0
(find-first-instance '(1 2 3 3 2) 3) ;; 2

(find-first-instance '(1 2 3) 9)

(define (find-first-instance ls x)
    (cond ((= (length ls) 0) #f) ;;  ((null? ls) #f)
        ((= x (car ls)) 
            (display "in (=x (car ls), list is: ")
            (display ls)
            (newline))
        (else (
                (display "in else, list is: ")
                (display ls)
                (newline)
                (+ 1 (find-first-instance (cdr ls) x))))))
(define (find-first-instance ls x)
    (display "Here is list: ")
    (display ls)
    (newline)
    (cond ((= (length ls) 0) #f) ;;  ((null? ls) #f)
        ((= x (car ls)) 0)
        (else (+ 1 (find-first-instance (cdr ls) x)))))
;; mine doesn't work if the item is not in the list
;; shido's answer
(define (position x ls)
  (position-aux x ls 0))

(define (position-aux x ls i)
  (cond
   ((null? ls) #f)
   ((eqv? x (car ls)) i)
   (else (position-aux x (cdr ls) (+ 1 i)))))
;; he said "A function", so I tried writing one, not one that calls another

;; tail recursive
;; his first recursion was at the end, so I am not clear how "tail recursion" is different than "recursion"
(define (fact-tail n)
    (fact-rec n n))

(define (fact-rec n p)
    (if (= n 1)
        p
        (let ((m (- n 1)))
            (fact-rec m (* p m)))))

;; exercise 2
;; my-reverse that reverse the order of list items. (Function reverse is pre-defined.)
;; from the other tutorial
(define (my-reverse s)
    (let loop ((s s) (r '()))
        (display "s is: ")
        (display s)
        (display "; r is: ")
        (display r)
        (newline)
      (if (null? s) r ;; if head is empty, return the end
          (let ((d (cdr s))) ;; set d to the rest of s
            (set-cdr! s r) ;; set the rest of s to r, keeping the head intact on s
            (loop d s))))) ;; go back to beginning with tail of s and head of s
;; shido's answer
(define (my-reverse ls)
  (my-reverse-rec ls ()))

(define (my-reverse-rec ls0 ls1)
  (if (null? ls0)
      ls1
      (my-reverse-rec (cdr ls0) (cons (car ls0) ls1))))

;; Summarizing items of a list consisting of numbers.
;; we did this one; what is the difference?
;; shido's answer
(define (my-sum-tail ls)
  (my-sum-rec ls 0))

(define (my-sum-rec ls n)
  (if (null? ls)
      n
      (my-sum-rec (cdr ls) (+ n (car ls)))))
(my-sum-tail '(10 11 5)) ;; 26
(my-sum-tail '(2 3 4)) ;; 9

;; Converting a string that represents a positive integer to the corresponding integer, i.e. "1232" → 1232. Input error check is not required.
;; Hint:
;; Character to number conversion is done by subtracting 48 from the ASCII codes of characters #\0 ... #\9. Function char->integer is available to get the ASCII code of a character.
;; Function string->list is available to convert string to a list of characters. 
(define (num-string-to-num ns)
    (rec-num-string (my-reverse (string->list ns)) 1))
(define (rec-num-string ns factor)
    (display-all "ns is " ns "; car ns: " (car ns) "; cdr ns: " (cdr ns) "; factor is: " factor)
    (if ((= (length ns) 1)) ;; ( (display "in the if with ns: ")(display ns)(newline)
            (* ns factor)
            ;; ((display "In the else")(newline)
                (+ (* (- (char->integer (car ns)) 48) factor) (rec-num-string (cdr ns) (* 10 factor)))))
(num-string-to-num "1234")
;; this is not working
;; shido's answer
(define (my-string->integer str)
  (char2int (string->list str) 0))

(define (char2int ls n)
    (display-all "ls: " ls ", n: " n)
  (if (null? ls)
      n
      (char2int (cdr ls) 
		(+ (- (char->integer (car ls)) 48)
		   (* n 10)))))

(my-string->integer "1234")

;;section 4 - named let -
(define (fact-let n)
    (let loop((n1 n) (p n))
        (display-all "n1: " n1 ", p:" p)
        (if (= n1 1)
            p
            (let ((m (- n1 1)))
                (loop m (* p m ))))))

;; exercise 3
;; do with let and loop
;;  A function that takes a list (ls) and an object (x) as arguments and returns a list removing x from ls.
;; shido's answer from above
(define (remove x ls)
  (if (null? ls)
      '()
      (let ((h (car ls)))
        ((if (eqv? x h)
            (lambda (y) y) ;; so this just returns itself?
            (lambda (y) (cons h y)))
         (remove x (cdr ls))))))

(define (remove-let x ls)
    (let loop((carls (car ls)) (cdrls (cdr ls)))
        (display-all "carls: " carls ", cdrls: " cdrls)
        (if (null? cdrls)
            carls
            (cond ((eqv? x carls)  
                    (begin
                        (display-all "in first cond, x: " x ", carls: " carls)
                        (loop '() cdrls)))
                  ((= x (car cdrls)) 
                      (begin
                          (display-all "in second cond, x: " x ", carls: " carls ", cdrls: " cdrls)
                            (loop carls (cdr cdrls))))
                  (else (begin
                          (display-all "in else, X: " x ", carls: " carls ", cdrls: " cdrls)
                          (display-all "attempting to call with: " (list carls (car cdrls)) " and " (cdr cdrls))
                          (loop (list carls (car cdrls)) (cdr cdrls))))))))
;; more concise
(define (remove-let x ls)
    (let loop((carls (car ls)) (cdrls (cdr ls)))
        (if (null? cdrls)
            carls
            (cond ((eqv? x carls) (loop '() cdrls) )
                  ((= x (car cdrls)) (loop carls (cdr cdrls)))
                  (else (loop (list carls (car cdrls)) (cdr cdrls)))))))
;; even more concise
(define (remove-let x ls)
    (let loop((carls (car ls)) (cdrls (cdr ls)))
        (cond ((null? cdrls) carls)
            ((eqv? x carls) (loop '() cdrls))
            ((eqv? x (car cdrls)) (loop carls (cdr cdrls)))
            (else (loop (list carls (car cdrls)) (cdr cdrls))))))
(remove-let 3 '(1 2 3 4 3))
(remove-let "a" '("q" "a" "r" "d" "a" "f"))
(define ls '(1 2 3 4 3))
;; it works, but see what shido comes up with - needs to be flattened
;; good to know you can loop from multiple places, less need for setters
;; Exception: attempt to apply non-procedure (2 3 4 3)
;; in Kawa: 
;; gnu.mapping.WrongArguments
;; 	at gnu.kawa.functions.ApplyToArgs.applyN(ApplyToArgs.java:162)
;; usually you have something like (some-var) instead of (some-func)
;; A function that takes a list (ls) and and an object (x) and returns the first position of x in ls. The position is counted from 0. If x is not found in ls, the function returns #f. 
;; shido's answer from above
(define (position x ls)
  (position-aux x ls 0))

(define (position-aux x ls i)
  (cond
   ((null? ls) #f)
   ((eqv? x (car ls)) i)
   (else (position-aux x (cdr ls) (+ 1 i)))))
;; done
(define (position-let x ls)
    (let loop ((lsa ls) (i 0))
        (cond
            ((null? lsa) #f)
            ((eqv? x (car lsa)) i)
            (else (loop (cdr lsa) (+ 1 i))))))
(position-let 1 '(1 2 3)) ;; 0
(position-let 3 '(1 2 3 3 2)) ;; 2

(position-let 9 '(1 2 3))

;; with named let
;; my-reverse that reverse the order of list items. (Function reverse is pre-defined.)
(define (my-reverse-let ls)
    (let loop ((lsa ls) (revls '()))
        (if (null? lsa) revls
            (loop (cdr lsa) (cons (car lsa) revls)))))
(my-reverse-let '(1 2 3 4)) ;; (4 3 2 1)
(my-reverse-let '('a 'b 'c 'd)) ;; ((quote d) (quote c) (quote b) (quote a))

;; Summarizing items of a list consisting of numbers.
(define (sum-list-let ls)
    (let loop ((lsa ls) (sum 0))
        (if (null? (cdr lsa)) (+ sum (car lsa))
            (loop (cdr lsa) (+ sum (car lsa))))))
(sum-list-let '(10 11 5))
(sum-list-let '(2 3 4))

;; Converting a string that represents a positive integer to the corresponding integer, i.e. "1232" → 1232. Input error check is not required.
;; Hint:
;;Character to number conversion is done by subtracting 48 from the ASCII codes of characters #\0 ... #\9. Function char->integer is available to get the ASCII code of a character.
;; Function string->list is available to convert string to a list of characters. 
(define (my-string->integer str)
  (char2int (string->list str) 0))

(define (char2int ls n)
    (display-all "ls: " ls ", n: " n)
  (if (null? ls)
      n
      (char2int (cdr ls) 
		(+ (- (char->integer (car ls)) 48)
		   (* n 10)))))

(my-string->integer "1234")
(define (string-to-number-let str)
    (let loop ((stra (string->list str)) (number 0))
        (display-all "stra: " stra ", number: " number)
        (if (null? stra) number
            (loop (cdr stra) (+ (* number 10) (- (char->integer (car stra)) 48) )))))
(string-to-number-let "1234")

;; section 5: letrec
;; letrec can make procedures into local variables
;; the "rec" is recursive, so letrec is like "let" for recursive functions
;; shido says: a name defined by letrec can refer itself in its definition

(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
		   (if (= n1 1)
		       p
		       (let ((m (- n1 1)))
			 (iter m (* p m)))))))     ; *
    (iter n n)))
;; from R7RS spec
(letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
  (even? 88))
;; functions calling each other
;; like in fact-letrec, "iter" is called at a few different places, not just at the tail
;;  Exercise 4: Write letrec version of exercise 2.
;; let with loop was soooo much better
;; my-reverse that reverse the order of list items. (Function reverse is pre-defined.)
(define (my-reverse-letrec ls)
    (letrec ((rev-letrec (lambda (lsa revls) ;; gotta start letrec w/2 parens, even though the 2nd seems extraneous
        (if (null? lsa) revls
            (rev-letrec (cdr lsa) (cons (car lsa) revls)))))) ;; letrec procs seem to end w/a LOT of parens
        (rev-letrec ls '())))
(my-reverse-letrec '(1 2 3 4)) ;; (4 3 2 1)
(my-reverse-letrec '('a 'b 'c 'd)) ;; ((quote d) (quote c) (quote b) (quote a))

;; Summarizing items of a list consisting of numbers.(
(define (sum-letrec ls)
    (letrec ((sum-inside-loop (lambda (inls sum)
        (if (null? (cdr inls)) (+ sum (car inls))
            (sum-inside-loop (cdr inls) (+ sum (car inls)))))))
        (sum-inside-loop ls 0))) ;; last call must be inside letrec

(sum-letrec '(10 11 5)) ;; 26
(sum-letrec '(2 3 4)) ;; 9
;; not a great use of letrec, I admit. But it still needs a condition that will end the recursion

;; Converting a string that represents a positive integer to the corresponding integer, i.e. "1232" → 1232. Input error check is not required.
;; Hint:
;; Character to number conversion is done by subtracting 48 from the ASCII codes of characters #\0 ... #\9. Function char->integer is available to get the ASCII code of a character.
;; Function string->list is available to convert string to a list of characters. 

;; frankly I think I will skip this one. Kind of tired of letrec already. 
;; this might be a good use of letrec
(define (remove-let x ls)
    (let loop((carls (car ls)) (cdrls (cdr ls)))
        (cond ((null? cdrls) carls)
            ((eqv? x carls) (loop '() cdrls))
            ((eqv? x (car cdrls)) (loop carls (cdr cdrls)))
            (else (loop (list carls (car cdrls)) (cdr cdrls))))))
(remove-let 3 '(1 2 3 4 3))
(remove-let "a" '("q" "a" "r" "d" "a" "f"))
(define (remove-letrec x ls)
    (letrec ((rm-letrec (lambda (carls cdrls)
        (cond ((null? cdrls) carls)
            ((eqv? x carls) (rm-letrec '() cdrls))
            ((eqv? x (car cdrls)) (rm-letrec carls (cdr cdrls)))
            (else (rm-letrec (list carls (car cdrls)) (cdr cdrls)))))))
        (rm-letrec (car ls) (cdr ls))))
(remove-letrec 3 '(1 2 3 4 3)) ;; ((1 2) 4) ;; still needs to be flattened
(remove-letrec 3 '(1 2 3 4)) ;; ((1 2) 4)
(remove-letrec "a" '("q" "a" "r" "d" "a" "f")) ;; (((q r) d) f)

;; another section 5: do expression
(define (fact-do n)
    (do ((n1 n (- n1 1)) (p n (* p (- n1 1)))) ((= n1 1) p)))

;; he says do is more complicated than named let, and I agree. Perhaps I will see what the overflowed stack has to offer
;; http://stackoverflow.com/questions/3199228/using-do-in-scheme

