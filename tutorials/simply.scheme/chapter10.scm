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
(define (tie-game? position))


