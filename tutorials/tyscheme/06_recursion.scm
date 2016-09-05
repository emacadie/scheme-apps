;; 06 recursion

;; a function that calls itself
;; chez scheme can handle (factorial 5000)
(define factorial
    (lambda (n)
        (if (= n 0) 1
            (* n (factorial (- n 1))))))

;; or two functions that call each other
(define is-even?
    (lambda (n)
        (if (= n 0) #t
            (is-odd? (- n 1)))))

(define is-odd?
    (lambda (n)
        (if (= n 0) #f
            (is-even? (- n 1)))))

;; letrec can make procedures into local variables
;; the "rec" is recursive, so letrec is like "let" for recursive functions
(letrec ((local-even? (lambda (n)
                (if (= n 0) #t
                    (local-odd? (- n 1)))))
        (local-odd? (lambda (n)
                (if (= n 0) #f
                    (local-even? (- n 1))))))
        (list (local-even? 23) (local-odd? 23)))

(letrec ((countdown (lambda (i)
                (if (= i 0) 'liftoff
                    (begin
                        (display i)
                        (newline)
                        (countdown (- i 1)))))))
    (countdown 10))

;; now a "named let" to do that loop more compactly
;; no parens to left of "countdown"
(let countdown ((i 10))
    (if (= i 0) 'liftoff
        (begin
            (display i)
            (newline)
            (countdown (- i 1)))))
;; so I guess this is like a macro

;; iteration
;; tail-call recursion: the recursive function can only call itself at the end
(define list-position
    (lambda (o l)
        (let loop ((i 0) (l l))
            (if (null? l) #f
                (if (eqv? (car l) o) i
                    (loop (+ i 1) (cdr l)))))))
(list-position '(1 2) '(3 4)) ;; #f
;; I don't quite get setting l to l, but whatever
;; because the first time, you need the whole thing? like below setting s to s in let
;; first time you take the whole s into the loop
(define reverse!
    (lambda (s)
        (let loop ((s s) (r '()))
            (display "s is: ")
            (display s)
            (display "; r is: ")
            (display r)
            (newline)
            (if (null? s) r
                (let ((d (cdr s)))
                    (set-cdr! s r)
                    (loop d s))))))
(reverse! '(1 2 3)) ;; (3 2 1)
;; gotta get the hang of car and cdr

;; mapping a procedure across a list
;; map applies a procedure to every element of a list
;; and returns a new list; you give it the keys, it gives you values
;; add2 is defined in 03_forms.scm
(map add2 '(1 2 3))

;; (for-each does the same, but does not return a value
(for-each display
    (list "one " "two " "buckle my shoe"))

(define g (map add2 '(1 2 3)))
g ;; (3 4 5)
(define e (for-each display
        (list "one " "two " "buckle my shoe")))
;; this is displayed by the above: one two buckle my shoe
e ;; this returns nothing

;;map can handle more than one-argument procedures
(map cons '(1 2 3) '(10 20 30)) ;; ((1 . 10) (2 . 20) (3 . 30))
(map + '(1 2 3) '(10 20 30)) ;; (11 22 33)

