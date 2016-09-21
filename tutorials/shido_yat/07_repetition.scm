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
;; Converting a string that represents a positive integer to the corresponding integer, i.e. "1232" → 1232. Input error check is not required.
;; Hint:
;; Character to number conversion is done by subtracting 48 from the ASCII codes of characters #\0 ... #\9. Function char->integer is available to get the ASCII code of a character.
;; Function string->list is available to convert string to a list of characters. 
(define (num-string-to-num ns)
    (rec-num-string (my-reverse (string->list ns)) 1))
(define (rec-num-string ns factor)
    (display "ns is ")
    (display ns)
    (display "; car ns: ")
    (display (car ns))
    (display "; cdr ns: " )
    (display (cdr ns))
    (display "; factor is: ")
    (display factor)
    (newline)
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
  (if (null? ls)
      n
      (char2int (cdr ls) 
		(+ (- (char->integer (car ls)) 48)
		   (* n 10)))))


;; named let
(define (fact-let n)
    (let loop((n1 n) (p n))
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
    (let loop((ls ls))
        (loop ))
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

