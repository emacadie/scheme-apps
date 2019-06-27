#lang simply-scheme

; Chapter 18: Lists

(require "more-simply.rkt")
; (require "chapter17.rkt"
(require (prefix-in ch17: "chapter17.rkt"))
; (prefix-in tcp: racket/tcp)
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
; I decided to skip this one
(define (make-node-list datum children)
  (list datum children))

(define (datum-list node)
  (car node))

(define (children-list node)
  (cdr node))

(define world-tree-2
  (make-node
   'world-list
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
                (make-node 'texas (cities '(austin dallas)))
                (make-node 'illinois (cities '(chicago springfield urbana)))
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


; How do we have to change the selectors so that everything still works? 
; Note: If you look at simply.scm, "make-node" is a redefinition of cons
;; datum is car, and children is cdr

;; 18.3  Write depth, a procedure that takes a tree as argument 
;; and returns the largest number of nodes connected through parent-child links. 
;; That is, a leaf node has depth 1; 
;; a tree in which all the children of the root node are leaves has depth 2. 
;; Our world tree has depth 4 
;; (because the longest path from the root to a leaf is, 
;; for example, world, country, state, city). 

(define (leaf? node)
  (null? (children node)))

(define (count-depth tree)
  (reduce max (ch17:flatten2 (count-depth-work tree 0 '()))))

(define (count-depth-work tree num num-list)
  (if (leaf? tree)
       (append num-list (+ 1 num))
      (count-depth-in-forest (children tree) num num-list)))

(define (count-depth-in-forest forest num num-list)
  (if (null? forest)
      num-list
      (list (count-depth-work (car forest) (+ 1 num) num-list)
            (count-depth-in-forest (cdr forest) num num-list))))

;; https://github.com/buntine/Simply-Scheme-Exercises/blob/master/18-trees/18-3.scm
(define (depth tree)
  (if (leaf? tree)
    1
    (find-depth tree 1)))

(define (find-depth tree d)
  (apply max
         (cons d
              (find-depth-in-forest (children tree) (+ 1 d)))))

(define (find-depth-in-forest tree d)
  (if (null? tree)
    '()
    (cons (find-depth (car tree) d)
          (find-depth-in-forest (cdr tree) d)))) 

;; we get the same answer
;; hard to test, hard to make quick trees

; 18.4  Write count-nodes, a procedure that takes a tree as argument 
; and returns the total number of nodes in the tree. 
; (Earlier we counted the number of leaf nodes.) 

(define (count-nodes tree)
  (count-nodes-tree tree 0))

(define (count-nodes-tree tree num)
  (if (leaf? tree)
      (+ 1 num)
      (count-nodes-in-forest (children tree) num)))

(define (count-nodes-in-forest forest num)
  (if (null? forest)
      (+ 1 num)
      (+ (count-nodes-tree (car forest) num)
         (count-nodes-in-forest (cdr forest) num ))))

;; https://github.com/buntine/Simply-Scheme-Exercises/blob/master/18-trees/18-4.scm
(define (count-nodes-b tree)
  (if (leaf? tree)
    1
    (+ 1 (count-nodes-in-forest-b (children tree)))))

(define (count-nodes-in-forest-b tree)
  (if (null? tree)
    0
    (+ (count-nodes-b (car tree))
       (count-nodes-in-forest-b (cdr tree)))))

;; I made some changes to world-tree and tested both our answers, 
;; and we get the same answer

;; 18.5  Write prune, a procedure that takes a tree as argument 
;; and returns a copy of the tree, but with all the leaf nodes of the original tree removed. 
;; (If the argument to prune is a one-node tree, in which the root node has no children, 
;; then prune should return #f because the result of removing the root node wouldn't be a tree.) 

(define small-world-tree
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
         (make-node
          'australia
          (list
           (make-node 'victoria (cities '(melbourne)))
           (make-node '(new south wales) (cities '(sydney)))
           (make-node 'queensland
                      (cities '(cairns (port douglas)))))))))

(define tiny-world-tree
  (make-node
   'world
   (list (make-node 'zimbabwe (cities '(harare hwange)))
         (make-node
          'australia
          (list
           (make-node 'victoria (cities '(melbourne)))
           (make-node '(new south wales) (cities '(sydney)))
           (make-node 'queensland
                      (cities '(cairns (port douglas)))))))))

(define (prune tree)
  (printf "-- Calling prune with tree: ~a\n" tree)
  (if (not (leaf? tree))
      (begin
        (printf "the tree is not a leaf \n")
        (+ 1 (prune-nodes (children tree))))
    ; null
      (begin
        (printf "the tree is a leaf \n")
        0)))

(define (prune-nodes tree)
  (printf "== Calling prune-nodes with tree: ~a \n" tree)
  (if (null? tree)
    0
    (begin
      (printf "Tree is not null, calling prune with ~a and prune-nodes with ~a \n" (car tree) (cdr tree))
      (+ (prune (car tree))
       (prune-nodes (cdr tree)))
      )))

(define (not-empty x)
  (not (empty? x)))

(define (prune-t tree)
  (printf "-- Calling prune-t with tree: ~a\n" tree)
  (if (not (leaf? tree))
      (begin
        (printf "the tree is not a leaf \n")
        (make-node (car tree) 
                   (filter not-empty (prune-nodes-t (children tree)))
))
    ; null
      (begin
        (printf "the tree is a leaf \n")
        null)))

(define (prune-nodes-t tree)
  (printf "== Calling prune-nodes-t with tree: ~a \n" tree)
  (if (null? tree)
    null
    (begin
      (printf "Tree is not null, calling prune-t with ~a and prune-nodes-t with ~a \n" (car tree) (cdr tree))
      (make-node (prune-t (car tree))
       (prune-nodes-t (cdr tree)))
      )))

(define (use-num x)
  (cond [(not (odd? x)) (printf "It's even: ~a \n" x)]
        [else (printf "In the else: ~a \n" x)]
)
)

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

