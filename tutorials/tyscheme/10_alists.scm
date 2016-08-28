;; 10 Alists and tables

;; from r6rs.org: Alist (for “association list”) should be a list of pairs
(define a1 '((a . 1) (b . 2) (c . 3)))
(assv 'b a1) ;; (b . 2)

;; eqv? is the default test, but we could use others
(defstruct table 
    (equ eqv?) 
    (alist '()))

(define lassoc
    (lambda (k al equ?)
        (let loop ((al al))
            (if (null? al) #f
                (let ((c (car a1)))
                    (if (equ? (car c) k) c
                        (loop (cdr al))))))))

;; args: table, key, optional default if not found
(define table-get
    (lambda (tbl k . d)
        (let ((c (lassoc k (table.alist tbl) (table.equ tbl))))
            (cond (c (cdr c))
                ((pair? d) (car d))))))

;; update a key's value in a table
(define table-put!
    (lambda (tbl k v)
        (let ((al (table.alist tbl)))
            (let ((c (lassoc k al (table.equ tbl))))
                (if c (set-cdr! c v)
                    (set!table.alist tbl (cons (cons k v) al)))))))

;; call a procedure on every key/value pair in the table
(define table-for-each
    (lambda (tbl p)
        (for-each
            (lambda (c)
                (p (car c) (cdr c)))
            (table.alist tbl))))


