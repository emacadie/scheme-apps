;; 03 Making Lists
;; cons makes a list with 2 objects
(cons 1 2) ;; (1 . 2) ;; one memory cell
(cons 3 (cons 1 2)) ;; (3 1 . 2) ;; two memory cells
;; you can mix data types
(cons #\a (cons 3 "Hello")) ;; (a 3 . Hello)

;; exercise 1
(cons "hi" "everybody") ;; (hi . everybody)
(cons 0 '()) ;; (0)
(cons 1 (cons 10 100)) ;; (1 10 . 100)
(cons 1 (cons 10 (cons 100 '()))) ;; (1 10 100)
(cons #\I (cons "saw" (cons 3 (cons "girls" '())))) ;; (I saw 3 girls) ;; kawa does not quote
;; in chez and guile: (#\I "saw" 3 "girls")
(cons "Sum of" (cons '(1 2 3 4) (cons "is" (cons 10 '())))) 
;; Kawa: (Sum of (1 2 3 4) is 10)
;; chez and guile: ("Sum of" (1 2 3 4) "is" 10)


