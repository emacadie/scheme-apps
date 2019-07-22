#lang simply-scheme

; Chapter 20 Input and Output

(require "more-simply.rkt")
(require (prefix-in ch17: "chapter17.rkt"))
(require (prefix-in ttt: "ttt.rkt"))

(butfirst '(This is Chapter 20 Input and Output))

;; Chapter 20 Input and Output

;; talks about effects vs values
;; introduces the "begin" special form
#|
Quote from the book defining functions and procedures:
Sequencing and side effects are radical departures from the idea of functional programming. 
In fact, we'd like to reserve the name function for something that computes and returns one value, with no side effects. 
"Procedure" is the general term for the thing that lambda returns—an embodiment of an algorithm. 
If the algorithm is the kind that computes and returns a single value without side effects, then we say that the procedure implements a function.
|#

;; There is some stuff about show, display and newline
;; Introduces strings
;; Introduces concept of "higher order procedure", like HOF with functions
;; so "for-each" is like "map" for when you need to print
;; unlike "map" it will not return a new list
;; plus, while the return list of "map" is in the correct order, 
;; it might not execute the elements in the right order
;; for HOP, we might care about order

(define (effect x)
  (show x)
  'done)

(define (value x)
  x)

(effect '(oh! darling))

(value '(oh! darling))

; (bf (effect '(oh! darling)))

; (bf (value '(oh! darling)))

(define (lots-of-effect x)
  (effect x)
  (effect x)
  (effect x))

(define (lots-of-value x)
  (value x)
  (value x)
  (value x))

;; some ttt stuff
; from chapter 10 in my "simply.scheme" sub-repo
(define (already-won? position me)
  (not (empty? (keep (lambda (triple) (= (appearances me triple) 3))
         (ttt:find-triples position)))))

(define (tie-game? position)
  (not (member? '_ position)))

;; from the book
(define (stupid-ttt position letter)
  (location '_ position))

(define (location letter word)
  (if (equal? letter (first word))
      1
      (+ 1 (location letter (bf word)))))

(define (add-move square letter position)
  (if (= square 1)
      (word letter (bf position))
      (word (first position) 
            (add-move (- square 1) letter (bf position)))))

(define (play-ttt x-strat o-strat)
  (play-ttt-helper x-strat o-strat '_________ 'x))

(define (play-ttt-helper x-strat o-strat position whose-turn)
  (cond [(already-won? position (ttt:opponent whose-turn))
         (list (ttt:opponent whose-turn) 'wins!)]
        [(tie-game? position) '(tie game)]
        [else (let ((square (if (equal? whose-turn 'x)
                                (x-strat position 'x)
                                (o-strat position 'o))))
                (play-ttt-helper x-strat
                                 o-strat
                                 (add-move square whose-turn position)
                                 (ttt:opponent whose-turn)))]))

(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (read))

(define (print-position position)
  (print-row (subword position 1 3))
  (show "-+-+-")
  (print-row (subword position 4 6))
  (show "-+-+-")
  (print-row (subword position 7 9))
  (newline))

(define (print-row row)
  (maybe-display (first row))
  (display "|")
  (maybe-display (first (bf row)))
  (display "|")
  (maybe-display (last row))
  (newline))

(define (maybe-display letter)
  (if (not (equal? letter '_))
      (display letter)
      (display " ")))

(define (subword wd start end)
  ((repeated bf (- start 1))
   ((repeated bl (- (count wd) end))
    wd)))

;; introduces read-line, but there is a read-line in Scheme

(module+ test
  (require rackunit)
  (check-true #t)
  (define (check-three-things-equal? result their-append-rsl my-append-rsl)
  (unless (and (check-equal? result their-append-rsl)
               (check-equal? result my-append-rsl))
    (fail-check)))





#|
  (check-equal?  )
|#

) ;; end module+ test 

