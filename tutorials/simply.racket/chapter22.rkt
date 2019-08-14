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
#|
(define (print-file-helper port)             ;; first version
  (let ((stuff (read-line port)))
    (if (eof-object? stuff)
	'done
	(begin (show-line stuff)
	       (print-file-helper port)))))
|#

;; transforming lines of a file
(define (file-map fn inname outname)
  (let ([inport (open-input-file inname)]
        [outport (open-output-file outname)])
    (file-map-helper fn inport outport)
    (close-input-port inport)
    (close-output-port outport)
    'done))

(define (file-map-helper fn inport outport)
  (let ([line (read-line inport)])
    (if (eof-object? line)
	'done
	(begin (show-line (fn line) outport)
	       (file-map-helper fn inport outport)))))

;; (file-map lastfirst "dddbmt" "dddbmt-reversed")
(define (lastfirst name)
  (se (word (last name) ",") (bl name)))

;; (file-map process-grades "grades" "results")
;; (print-file "grades")
;; (print-file "results")
(define (process-grades line)
  (se (first line)
      "total:"
      (accumulate + (bf line))
      "average:"
      (/ (accumulate + (bf line))
	 (count (bf line)))))

;; justify text
;; (file-map (lambda (sent) (justify sent 50)) "r5rs" "r5rs-just")
;; Note: The output file IS justified, however it does not appear so in REPL
;; Note 2: print-file-helper is redefined below to take care of that
(define (justify line width)
  (if (< (count line) 2)
      line
      (se (pad line
	       (- (count line) 1)
	       (extra-spaces width (char-count line))))))

(define (char-count line)
  (+ (accumulate + (every count line))      ; letters within words
     (- (count line) 1)))                   ; plus spaces between words

(define (extra-spaces width chars)
  (if (> chars width)
      0                                     ; none if already too wide
      (- width chars)))

(define (pad line chances needed)
  (if (= chances 0)                         ; only one word in line
      (first line)
      (let ([extra (quotient needed chances)])
        (word (first line)
              (spaces (+ extra 1))
              (pad (bf line) (- chances 1) (- needed extra))))))

(define (spaces n)
  (if (= n 0)
      ""
      (word " " (spaces (- n 1)))))

(define (print-file-helper port)
  (let ([stuff (read-string port)])
    (if (eof-object? stuff)
	'done
	(begin (show stuff)
	       (print-file-helper port)))))


;; up to Merging Two Files

;; delete the files we created
#|
(define (delete-our-files)
  (delete-file "songs2")
  (delete-file "dddbmt-reversed")
  (delete-file "results")
  (delete-file "r5rs-just")
)
|#
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

