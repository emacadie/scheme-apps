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

(define (print-chess-board the-board)
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
