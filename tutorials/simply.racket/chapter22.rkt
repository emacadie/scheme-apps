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

;; The previous section: They just treated each line as a string
;; and processed the string. That could have been in an earlier chapter.
;; up to Merging Two Files

(define (filemerge file1 file2 outfile)
  (let ([p1 (open-input-file file1)]
	    [p2 (open-input-file file2)]
	    [outp (open-output-file outfile)])
    (filemerge-helper p1 p2 outp (read-string p1) (read-string p2))
    (close-output-port outp)
    (close-input-port p1)
    (close-input-port p2)
    'done))

(define (filemerge-helper p1 p2 outp line1 line2)
  (cond [(eof-object? line1) (merge-copy line2 p2 outp)]
	    [(eof-object? line2) (merge-copy line1 p1 outp)]
	    [(before? line1 line2)
	     (show line1 outp)
	     (filemerge-helper p1 p2 outp (read-string p1) line2)]
	    [else (show line2 outp)
	          (filemerge-helper p1 p2 outp line1 (read-string p2))]))

(define (merge-copy line inp outp)
  (if (eof-object? line)
      #f
      (begin (show line outp)
	     (merge-copy (read-string inp) inp outp))))

#|
From the text about the input functions:
read ignores case and forces you to have parentheses in your file
read-line fixes those problems, but it loses spacing information 
read-string can read anything and always gets it right
But you can't do "first", "butfirst", etc with read-string: use read-line
If your file has scheme lists in it, use read
|#

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

