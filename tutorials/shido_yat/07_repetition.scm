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

;; A function that summarizes numbers in a list.
(define (sum-num ls)
    (if (null? ls)
        0
        (+ (car ls) (sum-num (cdr ls)))))
(sum-num '(1 2 3)) ;; 6
(sum-num '(500 800 9)) ;; 1309
;; A function that takes a list (ls) and an object (x) as arguments and returns a list removing x from ls.
(define (remove-item ls x)
    (if (= x (car ls))
        (cdr ls)
        (cons (car ls) (remove-item (cdr ls) x))))
(remove-item '(1 2 3 4) 3) ;; (1 2 4)
;; A function that takes a list (ls) and an object (x) and returns the first position of x in ls. The position is counted from 0. If x is not found in ls, the function returns #f. 
;; this one was hard
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

;; tail recursive
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

;; Summarizing items of a list consisting of numbers.
;; we did this one; what is the difference?
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
    (if ((= (length ns) 1) ( (display "in the if with ns: ")(display ns)(newline)(* ns factor)))
            ((display "In the else")(newline)
                (+ (* (- (char->integer (car ns)) 48) factor) (rec-num-string (cdr ns) (* 10 factor))))))
(num-string-to-num "1234")
;; this is not working


