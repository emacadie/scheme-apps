;; 11: Characters and Strings

;; (title-style "the cathedral and the bazaar")
;; ⇒ "The Cathedral And The Bazaar"

(define (title-style str)
    (let loop ((str-as-list (string->list str)) (prev-space #t) (new-str '()))
        (cond
            ((null? str-as-list) (list->string new-str))
            ((not prev-space) (loop (cdr str-as-list) (char=? #\space (car str-as-list)) (append new-str (list (car str-as-list)))))
            (else (loop (cdr str-as-list) (char=? #\space (car str-as-list)) (append new-str (list (char-upcase (car str-as-list)))))))))
;; let loop to the rescue again! If I knew about "append" for section 7, I think it would have been easier
;; I admit, I might be relying a bit much on let loop
;; But I think it helps understand the functional way a bit: You are creating new fields with each iteration

;; shido's answer:
(define (identity x) x)

(define (title-style str)
  (let loop ((ls (string->list str))
	     (w #t)
	     (acc '()))
    (if (null? ls)
	(list->string (reverse acc))
	(let ((c (car ls)))
	  (loop (cdr ls)
		(char-whitespace? c)
		(cons ((if w char-upcase identity) c) acc))))))

;; shido's second answer
;;; Another answer, You can assign caps to the string.
(define (title-style str)
  (let ((n (string-length str)))
    (let loop ((w #t) (i 0))
      (if (= i n)
	  str
	  (let ((c (string-ref str i)))
	    (if w (string-set! str i (char-upcase c)))
	    (loop (char-whitespace? c) (1+ i)))))))

