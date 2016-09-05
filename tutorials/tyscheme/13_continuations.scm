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

;; check fringes on a list: same elements in same order
(define flatten
    (lambda (tree)
        (cond ((null? tree) '())
            ((pair? (car tree))
                (append (flatten (car tree))
                    (flatten (cdr tree))))
            (else
                (cons (car tree)
                    (flatten (cdr tree)))))))

(define same-fringe?
    (lambda (tree1 tree2)
        (let loop((ftree1 (flatten tree1))
                (ftree2 (flatten tree2)))
            (cond ((and (null? ftree1) (null? ftree2)) #t)
                ((or (null? ftree1) (null? ftree2)) #f)
                ((eqv? (car ftree1) (car ftree2))
                (loop (cdr ftree1) (cdr ftree2)))
            (else #f)))))
(same-fringe? '(1 (2 3)) '((1 2) 3)) ;; #t
(same-fringe? '(1 2 3) '(1 (3 2))) ;; #f

;; same, using call/cc
(define print-breakpoint
    (lambda (message continuation)
        (display message)
        (display continuation)
        (newline)))
(define tree->generator
    (lambda (tree)
        (let ((caller '*))
            (letrec
                ((generate-leaves
                        (lambda ()
                            (let loop ((tree tree))
                                (print-breakpoint "here is tree in loop: " tree)
                                (cond ((null? tree) 'skip)
                                    ((pair? tree)
                                        (print-breakpoint "calling loop with tree: " tree)
                                        (loop (car tree))
                                        (loop (cdr tree)))
                                    (else
                                        (call/cc
                                            (lambda (rest-of-tree)
                                                (print-breakpoint "in call/cc with rest-of-tree: " rest-of-tree)
                                                (set! generate-leaves ;; so here it's a variable?
                                                    (lambda()
                                                        (rest-of-tree 'resume)))
                                                (caller tree))))))
                                        (caller '()))))
                            (lambda ()
                                (call/cc
                                    (lambda (k)
                                        (print-breakpoint "In call/cc with k: " k)
                                        (set! caller k)
                                        (generate-leaves))))))))

(define same-fringe?
    (lambda (tree1 tree2)
        (let ((gen1 (tree->generator tree1))
                (gen2 (tree->generator tree2)))
            (let loop()
                (print-breakpoint "loop in same-fringe? with gen1: " gen1)
                (print-breakpoint "loop in same-fringe? with gen2: " gen2)
                (let ((leaf1 (gen1))
                        (leaf2 (gen2)))
                    (print-breakpoint "let in loop in same-fringe? with leaf1: " leaf1)
                    (print-breakpoint "let in loop in same-fringe? with leaf2: " leaf2)
                    (if (eqv? leaf1 leaf2)
                        (if (null? leaf1) #t (loop))
                        #f))))))

(same-fringe? '(1 (2 3)) '((1 2) 3)) ;; #t
(same-fringe? '(1 2 3) '(1 (3 2))) ;; #f

;; doesn't do much
(define call-tg
    (lambda (tree)
        (tree->generator tree)))
(call-tg '(1 (2 3)))
(call-tg '((1 2) 3)) 
(call-tg '(1 2 3))
(call-tg '(1 (3 2)))

;; define-macro for kawa and guile, define-syntax for chez
;; note: I could not get this to work in guile or chez
(define-syntax coroutine
    (lambda (x . body)
        `(letrec ((+local-control-state
                    (lambda (,x) ,@body))
                (resume
                    (lambda (c v)
                        (call/cc
                            (lambda (k)
                                (set! +local-control-state k)
                                (cv))))))
            (lambda (v)
                (+local-control-state v)))))


(define make-matcher-coroutine
    (lambda (tree-cor-1 tree-cor-2)
        (coroutine dont-need-an-init-arg
            (let loop()
                (let ((leaf1 (resume tree-cor-1 'get-a-leaf))
                        (leaf2 (resume tree-cor-2 'get-a-leaf)))
                    (if (eqv? leaf1 leaf2)
                        (if (null? leaf1) #t (loop))
                        #f))))))

(define make-leaf-gen-coroutine
    (lambda (tree matcher-cor)
        (coroutine dont-need-an-init-arg
            (let loop ((tree tree))
                (cond ((null? tree) 'skip)
                    ((pair? tree)
                        (loop (car tree))
                        (loop (cdr tree)))
                    (else
                        (resume matcher-cor tree))))
            (resume matcher-cor '()))))

(define same-fringe?
    (lambda (tree1 tree2)
        (letrec ((tree-cor-1 (make-leaf-gen-coroutine tree1 (lambda (v) (matcher-cor v))))
                (tree-cor-2 (make-leaf-gen-coroutine tree2 (lambda (v) (matcher-cor v))))
                (matcher-cor (make-matcher-coroutine (lambda (v) (tree-cor-1 v)) (lambda (v) (tree-cor-2)))))
            (matcher-cor 'start-ball-rolling))))

