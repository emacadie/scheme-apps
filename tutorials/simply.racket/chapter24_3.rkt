#|
24.3  Make a spreadsheet containing the values from Pascal's triangle: 
Each element should be the sum of the number immediately above it and the number immediately to its left, 
except that all of column a should have the value 1, and all of row 1 should have the value 1. 
(load "chapter24_3.rkt")
|#

(put 1 a1)
(put 1 a)
(put 1 1)
; (put (+ (cell a <0) (cell b <0 ))  b)
; this is good: (put (+ a2 b1) b2)
;; could also use * for same row/column
(put (+ (cell <1 <0) (cell <0 <1)) b)
(put (+ (cell <1 <0) (cell <0 <1)) c)
(put (+ (cell <1 <0) (cell <0 <1)) d)
(put (+ (cell <1 <0) (cell <0 <1)) e)
(put (+ (cell <1 <0) (cell <0 <1)) f)


