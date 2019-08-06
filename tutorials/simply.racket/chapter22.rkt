#lang simply-scheme

; Chapter 22 Files

(require "more-simply.rkt")
(require (prefix-in ch17: "chapter17.rkt"))
(require (prefix-in ttt: "ttt.rkt"))


;; Chapter 22 Files Input and Output

;; show-line is like show but removes parens
;; Why does everyone hate the parens?
;; use ports with file
 (let ((port (open-output-file "songs2")))
    (show-line '(all my loving) port)
    (show-line '(ticket to ride) port)
    (show-line '(martha my dear) port)
    (close-output-port port))

(define (get-song n)
  (let ((port (open-input-file "songs2")))
    (skip-songs (- n 1) port)
    (let ((answer (read-line port)))
      (close-input-port port)
      answer)))

(define (skip-songs n port)
  (if (= n 0)
      'done
      (begin (read-line port)
	     (skip-songs (- n 1) port))))

(define (print-file name)
  (let ((port (open-input-file name)))
    (print-file-helper port)
    (close-input-port port)
    'done))

(define (print-file-helper port)             ;; first version
  (let ((stuff (read-line port)))
    (if (eof-object? stuff)
	'done
	(begin (show-line stuff)
	       (print-file-helper port)))))







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

