;; 13 Lists

;; 2 association lists
;; association lists should be dot pairs or ordinal lists
'((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 0))
'((1 2 3) (4 5 6) (7 8 9))

(define wc '((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 0)))
(assq 'hi wc) ;; (hi . 3)
(assq 'you wc) ;; (you . 0)
(assq 'i wc) ;; #f

(define n '((1 2 3) (4 5 6) (7 8 9)))
(assv 1 n) ;; (1 2 3)
(assv 4 n) ;; (4 5 6)
(assv 8 n) ;; #f

;; 3 Hash Tables
;; convert keys to integers by a hash function and store values at addresses indicated by integers
;; see http://srfi.schemers.org/srfi-69/srfi-69.html
;; see https://www.gnu.org/software/kawa/Hash-tables.html

;; shido's password program - I might skip for now
(define (char-graphic? char)
    (or (char-alphabetic? char) (char-numeric? char)))
(define (skip-char? c)
    (or (not (char-graphic? c)) (memv c '(#\: #\; #\' #\" #\`))))
(define (ss-make-alist c alist)
    (let ((p (assv c alist)))
        (if p
            (begin
                (set-cdr! p (+ 1 (cdr p)))
                alist)
            (cons (cons c 1) alist))))


;;

