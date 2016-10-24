;; 10 - Assignment

;; functions that change a value usually end w/exclamation point

;; 2 set!
(define var 1)
var ;; 1
(set! var (* var 10))
var ;; 10
(set! var 33) ;; you don't need to use the field as part of the arg to set!
var ;; 33
(let ((i 1))
    (set! i (+ i  3))
    i)
;; 4

;; 3 lexical scope
(define bank-account
    (display-all "balance in bank-account is " balance)
    (let ((balance 10))
        (lambda (n)
            (set! balance (+ balance n))
            balance)))
(bank-account 20) ;; 30
(bank-account -25) ;; -5
;; so it keeps the value through multiple calls

;; write a procedure that returns a procedure and use it
(define (make-bank-account balance)
    (lambda (n)
        (display-all "balance in make-bank-account is " balance)
        (set! balance (+ balance n))
        balance))

(define gates-bank-account (make-bank-account 10))
(gates-bank-account 50) ;; 60

(gates-bank-account -55) ;; 5

(define torvalds-bank-account (make-bank-account 100))
(torvalds-bank-account -70) ;; 30
(torvalds-bank-account 300) ;; 330
(gates-bank-account 100) ;; 105
(gates-bank-account 0) ;; 105

;; Exercise 1
;; Modify make-bank-account so that withdrawing more than balance causes error.
;; hint: Use begin to group more than one S-expressions. 
(define (make-safe-account balance)
    (lambda (n)
        (if (not (negative? (+ balance n)))
            (begin
                (set! balance (+ balance n))
                balance)
            #f)))

(define ellison-account (make-safe-account 10))
(ellison-account 4)
(define bezos-account (make-safe-account 20))
;; shido's is a bit better: calculate once
(define (make-bank-account amount)
  (lambda (n)
    (let ((m (+ amount n)))
      (if (negative? m)
	  'error
	  (begin
	    (set! amount m)
	    amount)))))

;; 4 destructive operations on lists
(define tree '((1 2) (3 4 5) (6 7 8 9)))
tree ;; ((1 2) (3 4 5) (6 7 8 9))
(set-car! (car tree) 100) ;; will not work in kawa: cannot modify read-only pair, works in guile
tree ;; ((100 2) (3 4 5) (6 7 8 9))

;; shido's queue
(define (make-queue)
    (cons '() '()))

(define (enqueue! queue obj)
    (let ((lobj (cons obj '())))
        (if (null? (car queue))
            (begin
                (set-car! queue lobj)
                (set-cdr! queue lobj))
            (begin
                (set-cdr! (cdr queue) lobj)
                (set-cdr! queue lobj)))
        (car queue)))

(define (dequeue! queue)
    (let ((obj (car (car queue))))
        (set-car! queue (cdr (car queue)))
        obj))

(define q (make-queue))
(enqueue! q 'a) ;; (a)
(enqueue! q 'b) ;; (a b)
(enqueue! q 'c) ;; (a b c)
q ;; ((a b c) c) ;; not sure why Kawa added the extra c
q ;; ((a b c) c)
(dequeue! q) ;; a
q ;; ((b c) c)

