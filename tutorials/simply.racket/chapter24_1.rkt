#lang simply-scheme

#|
24.1  Set up a spreadsheet to keep track of the grades in a course. 
Each column should be an assignment; each row should be a student. 
The last column should add the grade points from the individual assignments. 
You can make predictions about your grades on future assignments and see what overall numeric grade each prediction gives you. 
|#
; assignments
(put "Quiz1" b1)
(put "Quiz2" c1)
(put "Quiz3" d1)
(put "Quiz4" e1)
(put "Sum" f1)
; students
(put "Archer" a2)
(put "T'Pol" a3)
(put "Trip" a4)
(put "Hoshi" a5)

; Archer
(put 22 b2)
(put 22 c2)
(put 22 d2)
(put 22 e2)
(put (+ (cell b) (cell c) (cell d) (cell e)) f2)
; T'Pol
(put 33 b3)
(put 33 c3)
(put 33 d3)
(put 33 e3)
(put (+ (cell b) (cell c) (cell d) (cell e)) f3)

; Trip
(put 44 b4)
(put 44 c4)
(put 44 d4)
(put 44 e4)
(put (+ (cell b) (cell c) (cell d) (cell e)) f4)

; Hoshi
(put 55 b5)
(put 55 c5)
(put 55 d5)
(put 55 e5)
(put (+ (cell b) (cell c) (cell d) (cell e)) f5)

;; (module+ test
;;   (require rackunit)
;;   (check-true #t)
;;    (printf "(sphere-volume 10):  ~a \n" (sphere-volume 10)) 
;; )
(module test racket/base)

