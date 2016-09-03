;; 13 continuations

;; does not work in Kawa
;; returns "4" in chez and guile
(+ 1 (call/cc
        (lambda (k)
            (+ 2 (k 3)))))

;; this does not work anywhere
(define r #f)

(+1 (call/cc
        (lambda (k)
            (set! r k)
            (+ 2 (k 3)))))

;; multiply a list of numbers
(define list-product
    (lambda (s)
        (let recur ((s s))
            (if (null? s ) 1
                (* (car s) (recur (cdr s)))))))

(list-product '( 2 3 4)) ;; 24

(define list-product
    (lambda (s)
        (call/cc
            (lambda (exit)
                (let recur ((s s))
                    (if (null? s ) 1
                        (if (= (car s) 0) (exit 0)
                            (* (car s) (recur (cdr s))))))))))

(define flatten
    (lambda (tree)
        (cond ((null? tree) '())
            ((pair? (car tree))
                (append (flatten (car tree))
                    (flatten (cdr tree))))
            (else
                (cons (car tree)
                    (flaten (cdr tree)))))))


