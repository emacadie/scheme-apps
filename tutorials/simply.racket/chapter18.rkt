#lang simply-scheme

; Chapter 18: Lists

(require "more-simply.rkt")

(butfirst '(This is chapter 18 trees))

;; Chapter 18 Trees


#|
Concepts in this chapter:
Trees: root node, branch node, leaf node
Each node has a datum (word or sentence), and zero or more children.
Trees are recursive: Trees have trees in them, man. Dave's not here. Mind blown.
Nodes can be parents, siblings, children

Mutual recursion
initialization procedure calls its helper procedure
helper procedure calls itself and the initialization procedure
init procedure takes tree as its arg, checks if a node is a leaf (or some other check)
else it calls the helper procedure with the tree as a list
helper procedure: if the tree is null, return false
else: call init procedure on the car of tree, call itself on cdr of tree
That is the most general pattern

Trees as lists: "In other words, a tree is a list whose first element is the datum and whose remaining elements are subtrees."

|#

;; Here is the world program
(define (leaf datum)
  (make-node datum '()))

(define (cities name-list)
  (map leaf name-list))

(define world-tree
  (make-node
   'world
   (list (make-node
          'italy
          (cities '(venezia riomaggiore firenze roma)))
         (make-node
          '(united states)
          (list (make-node
                 'california
                 (cities '(berkeley (san francisco) gilroy)))
                (make-node
                 'massachusetts
                 (cities '(cambridge amherst sudbury)))
                (make-node 'ohio (cities '(kent)))))
         (make-node 'zimbabwe (cities '(harare hwange)))
         (make-node 'china
		    (cities '(beijing shanghai guangzhou suzhou)))
         (make-node
          '(great britain)
          (list 
           (make-node 'england (cities '(liverpool)))
           (make-node 'scotland
		      (cities '(edinburgh glasgow (gretna green))))
           (make-node 'wales (cities '(abergavenny)))))
         (make-node
          'australia
          (list
           (make-node 'victoria (cities '(melbourne)))
           (make-node '(new south wales) (cities '(sydney)))
           (make-node 'queensland
                      (cities '(cairns (port douglas))))))
         (make-node 'honduras (cities '(tegucigalpa))))))


;; Here is the operator/operand program
(define (parse expr)
  (parse-helper expr '() '()))

(define (parse-helper expr operators operands)
  (cond [(null? expr)
         (if (null? operators)
             (car operands)
             (handle-op '() operators operands))]
        [(number? (car expr))
         (parse-helper (cdr expr)
                       operators
                       (cons (make-node (car expr) '()) operands))]
        [(list? (car expr))
         (parse-helper (cdr expr)
                       operators
                       (cons (parse (car expr)) operands))]
        [else (if (or (null? operators)
                      (> (precedence (car expr))
                         (precedence (car operators))))
                  (parse-helper (cdr expr)
                                (cons (car expr) operators)
                                operands)
                  (handle-op expr operators operands))]))

(define (handle-op expr operators operands)
  (parse-helper expr
                (cdr operators)
                (cons (make-node (car operators)
                                 (list (cadr operands) (car operands)))
                      (cddr operands))))

(define (precedence oper)
  (if (member? oper '(+ -)) 1 2))

(define (compute tree)
  (if (number? (datum tree))
      (datum tree)
      ((function-named-by (datum tree))
       (compute (car (children tree)))
       (compute (cadr (children tree))))))

(define (function-named-by oper)
  (cond [(equal? oper '+) +]
        [(equal? oper '-) -]
        [(equal? oper '*) *]
        [(equal? oper '/) /]
        [else (error "no such operator as" oper)]))


;; exercises
; 18.1  What does
; ((SAN FRANCISCO))
; mean in the printout of world-tree? Why two sets of parentheses? 
;; It has two sets of parentheses because it has more than one word. It is a list.

; 18.2  Suppose we change the definition of the tree constructor 
; so that it uses list instead of cons:

(define (make-node-list datum children)
  (list datum children))

(define (datum-list node)
  (car node))

(define (children-list node)
  (cdr node))

(define world-tree-list
  (make-node-list
   'world-list
   (list (make-node
          'italy
          (cities '(venezia riomaggiore firenze roma)))
         (make-node-list
          '(united states)
          (list (make-node
                 'california
                 (cities '(berkeley (san francisco) gilroy)))
                (make-node-list
                 'massachusetts
                 (cities '(cambridge amherst sudbury)))
                (make-node-list 'ohio (cities '(kent)))))
         (make-node-list 'zimbabwe (cities '(harare hwange)))
         (make-node-list 'china
		    (cities '(beijing shanghai guangzhou suzhou)))
         (make-node-list
          '(great britain)
          (list 
           (make-node-list 'england (cities '(liverpool)))
           (make-node-list 'scotland
		      (cities '(edinburgh glasgow (gretna green))))
           (make-node-list 'wales (cities '(abergavenny)))))
         (make-node-list
          'australia
          (list
           (make-node-list 'victoria (cities '(melbourne)))
           (make-node-list '(new south wales) (cities '(sydney)))
           (make-node-list 'queensland
                      (cities '(cairns (port douglas))))))
         (make-node-list 'honduras (cities '(tegucigalpa))))))


; How do we have to change the selectors so that everything still works? 



(module+ test
  (require rackunit)
  (check-true #t)
  (define (check-three-things-equal? result their-append-rsl my-append-rsl)
  (unless (and (check-equal? result their-append-rsl)
               (check-equal? result my-append-rsl))
    (fail-check)))

) ;; end module+ test 
  ; (printf ": ~a \n"  )
  ; (check-equal?  "Error for: ")

