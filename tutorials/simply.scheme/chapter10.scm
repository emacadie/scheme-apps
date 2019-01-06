;; chapter 10
;; from "Teach Yourself Scheme"
(define display3
    (lambda (arg1 arg2 arg3)
        (display arg1)
        (display " ")
        (display arg2)
        (display " ")
        (display arg3)
        (newline)))

(define (print-ttt-board the-board)
  (display (item 1 the-board))
  (display (item 2 the-board))
  (display (item 3 the-board))
  (newline)
  (display (item 4 the-board))
  (display (item 5 the-board))
  (display (item 6 the-board))
  (newline)
  (display (item 7 the-board))
  (display (item 8 the-board))
  (display (item 9 the-board))
  (newline))

;; now you can send a board like '_xo-ox_-___ and the delimiter '- (or any delimiter)
;; like this: (simplify-board '__xoxooxo '-)
(define (simplify-board board delimiter)
  (keep (lambda ( next-letter )
          (not (member? next-letter delimiter))) board))

(define (simplify-ttt board delimiter me)
  (ttt (simplify-board board delimter) me))


;; first, load ttt.scm
;; 10.1  The ttt procedure assumes that nobody has won the game yet. 
;; What happens if you invoke ttt with a board position in which some player has already won? 
;; Try to figure it out by looking through the program before you run it.
;; A complete tic-tac-toe program would need to stop when one of the two players wins. 
;; Write a predicate already-won? that takes a board position and a letter (x or o) as its arguments and returns #t if that player has already won.

;; (define current-triples (find-triples 'oxxoxoxo_))
; that's 'oxx-oxo-xo_
; (oxx oxo xo9 oox xxo xo9 ox9 xxx)
; (print-ttt-board 'oxxoxoxo_ )
;; they use "me" for the letter that the computer is playing
(define (already-won? position me)
  (not (empty? (keep (lambda (triple) (= (appearances me triple) 3))
         (find-triples position)))))

;; the other two solutions that I linked to had something better
;; so I will put it here
(define (better-already-won? position me)
  (member? (word me me me) (find-triples position)))

;; 10.2  The program also doesn't notice when the game has ended in a tie, that is, when all nine squares are already filled. 
;; What happens now if you ask it to move in this situation?
;; now it blows up
;; Write a procedure tie-game? that returns #t in this case. 
; 'oxoxoxoxox
;; we could use something like (number? (first (sort-digits (accumulate word (find-triples 'xxoxo_ox_)))))
;; or (member? '_ 'xoxoxoxox)
(define (tie-game? position)
  (not (member? '_ position)))

;;  10.3  A real human playing tic-tac-toe would look at a board like this:
; o x o
; o x x
; x o
; and notice that it's a tie, rather than moving in square 9. Modify tie-game? from Exercise 10.2 to notice this situation and return #t.
; (Can you improve the program's ability to recognize ties even further? What about boards with two free squares?)
;; board is ; 'oxo-oxx-xo_ 'oxooxxxo_
;; '____x____
; i-can-win? i-can-fork? i-can-advance?
(define (new-tie-game? position)
  (new-tie-with-triples (find-triples position)))
;; use authors' functions. I tried to use keep, but I could not get it to work
(define (new-tie-with-triples triples)
  (and (neither-can-win triples) (neither-can-fork triples) (neither-can-advance triples)))

;; there is probably a way to do this with every or something
(define (neither-can-win triples)
  (and (not (i-can-win? triples 'x))
       (not (i-can-win? triples 'o))))

(define (neither-can-advance triples)
  (and (not (i-can-advance? triples 'x))
       (not (i-can-advance? triples 'o))))

(define (neither-can-fork triples)
  (and (not (i-can-fork? triples 'x))
       (not (i-can-fork? triples 'o))))

;; does not work
(define (side-cannot-move triples side)
  (keep #t (lambda (procedure) (procedure triples side)) '(i-can-win? i-can-fork? i-can-advance)))

(keep #t (lambda (procedure) (procedure (find-triples 'oxooxxxo_) 'x)) '(i-can-win? i-can-fork? i-can-advance))
;; do not work
(every (lambda (procedure) (procedure (find-triples 'oxooxxxo_) 'x)) '(i-can-win? i-can-fork? i-can-advance))
(every (lambda (fn) (fn (find-triples 'oxooxxxo_) 'x)) '(i-can-win? i-can-fork? i-can-advance?))

