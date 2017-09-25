;; https://people.eecs.berkeley.edu/~bh/ssch7/variables.html
;; "Like cond, let uses parentheses both with the usual meaning (invoking a procedure) and to group sub-arguments that belong together."
;; some Lispers/Schemers do not like that Clojure uses other punctuation for grouping (like [] for vectors)
;; maybe that is a better. It is a bit clearer.
;; Says to think of it this way: (let variables body)
;; Also:
;; (define x (+ x 3)) 
;; not allowed in Scheme
;; redefining is bad, mm-kay?

;; 7.1  The following procedure does some redundant computation.
(define (gertrude wd)
  (se (if (vowel? (first wd)) 'an 'a)
      wd
      'is
      (if (vowel? (first wd)) 'an 'a)
      wd
      'is
      (if (vowel? (first wd)) 'an 'a)
      wd))
;; 7.1  The following procedure does some redundant computation.
(define (stein wd)
  (let ((article (if (vowel? (first wd)) 'an 'a)))
    (se article wd 'is article wd 'is article wd )))
;; 7.1  The following procedure does some redundant computation.
;; 7.1  The following procedure does some redundant computation.

;;  7.2  Put in the missing parentheses:

(let ((pi 3.14159)
       (pie '(lemon meringue))) 
  (se 'pi is pi 'but 'pie is pie))
;; I looked at another solution online, but I guess this does not work in Chicken
;; works in guile

;; 7.3  The following program doesn't work. Why not? Fix it.

;; (define (superlative adjective word)
;;   (se (word adjective 'est) word))

;; "word" is already a function in Simply Scheme
;; change to arg-word 
(define (superlative adjective arg-word)
  (se (word adjective 'est) arg-word))

 7.4  What does this procedure do? Explain how it manages to work.

(define (sum-square a b)
  (let ((+ *)
        (* +))
    (* (+ a a) (+ b b))))
;; it redefines "+" as "*" and "*" as "+"
;; Frankly, not a good idea. 

