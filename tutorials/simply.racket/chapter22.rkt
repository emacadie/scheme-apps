#lang simply-scheme

; Chapter 22 Files

(require "more-simply.rkt")
(require (prefix-in ch17: "chapter17.rkt"))
(require (prefix-in ttt: "ttt.rkt"))
(require (prefix-in srfi-13: srfi/13))

;; Chapter 22 Files Input and Output

;; show-line is like show but removes parens
;; Why does everyone hate the parens?
;; use ports with file
 (let ([port (open-output-file "songs2")])
    (show-line '(all my loving) port)
    (show-line '(ticket to ride) port)
    (show-line '(martha my dear) port)
    (close-output-port port))

(define (get-song n)
  (let ([port (open-input-file "songs2")])
    (skip-songs (- n 1) port)
    (let ([answer (read-line port)])
      (close-input-port port)
      answer)))

(define (skip-songs n port)
  (if (= n 0)
      'done
      (begin (read-line port)
	     (skip-songs (- n 1) port))))

(define (print-file name)
  (let ([port (open-input-file name)])
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
  (let ([p1   (open-input-file file1)]
        [p2   (open-input-file file2)]
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
(delete-our-files '("songs2" "dddbmt-reversed" "results" "r5rs-just"
                    "all-states-we-have"))
|#
(define (delete-if-exists file-name)
  (when (file-exists? file-name)
    (delete-file file-name)))

(define (delete-our-files list-of-files)
  (for-each delete-if-exists list-of-files))


; 22.1  Write a concatenate procedure that takes two arguments: 
; a list of names of input files, and one name for an output file. 
; The procedure should copy all of the input files, in order, into the output file. 
 #|

It writes this to file:
(Alabama)
(Alaska)
(Arizona)
(Arkansas)
(Idaho)
(Illinois)
etc
I'll take it.

(concat-files
'("a-states" "i-states" "n-states")
"all-states-we-have")
|#

(define (concat-map fn inname outport)
  (let ([inport (open-input-file inname)])
    (concat-map-helper fn inport outport)
    (close-input-port inport)
    'done))

(define (concat-map-helper fn inport outport)
  (let ([line (read-line inport)])
    (if (eof-object? line)
	'done
	(begin (show (fn line) outport)
	       (concat-map-helper fn inport outport)))))

(define (concat-files input-files output-file)
  (let ([outport (open-output-file output-file)])
    (for-each (lambda (next-in)
             (concat-map sentence next-in outport))
           input-files)
    (close-output-port outport)))

;; 22.2  Write a procedure to count the number of lines in a file. 
;; It should take the filename as argument and return the number.

(define (file-count-helper inport count)
  (let ([line (read-line inport)])
    (if (eof-object? line)
        count
        (file-count-helper inport (+ count 1)))))

;; frankly, I think I like how Clojure has "let"
;; work like "let" and "let*" in Scheme/Racket
(define (count-file-lines file-name)
  (let* ([inport  (open-input-file file-name)]
         [counter (file-count-helper inport 0)])
    (close-input-port inport)
    counter))

;; 22.3  Write a procedure to count the number of words in a file. 
;; It should take the filename as argument and return the number. 
(define (file-count-word-helper inport count)
  (let ([line (read-line inport)])
    (if (eof-object? line) 
        count
        (file-count-word-helper inport 
                                (+ count 
                                   (length line))))))

(define (count-file-words file-name)
    (let* ([inport  (open-input-file file-name)]
           [counter (file-count-word-helper inport 0)])
    (close-input-port inport)
    counter))

;; 22.4  Write a procedure to count the number of characters in a file, including space characters. 
;; It should take the filename as argument and return the number. 
(define (file-count-chars-helper inport count)
  (let ([line (read-string inport)])
    (if (eof-object? line) 
        count
        (file-count-chars-helper inport 
                                 (+ count 
                                    1 ;; must add 1 for newline 
                                    (string-length line))))))

(define (count-file-chars file-name)
    (let* ([inport  (open-input-file file-name)]
           [counter (file-count-chars-helper inport 0)])
    (close-input-port inport)
    counter))

;; 22.5  Write a procedure that copies an input file to an output file 
;; but eliminates multiple consecutive copies of the same line. 
#|
That is, if the input file contains the lines

John Lennon
Paul McCartney
Paul McCartney
George Harrison


Paul McCartney
Ringo Starr

then the output file should contain

John Lennon
Paul McCartney
George Harrison

Paul McCartney
Ringo Starr
|#
; (remove-repeated-lines "beatles-input" "beatles-output")
; I could add a parameter for when the file is read for first time
(define (remove-repeated-lines-helper inport outport prev-line)
  (let ([line (read-string inport)])
    ; (display-all "in helper with prev-line: " prev-line " and line: " line)
      (cond [(eof-object? line) 'done]
            [(null? prev-line) (begin
                                 (show line outport)
                                 (remove-repeated-lines-helper inport 
                                                               outport 
                                                               line))]
            [(equal? prev-line line) (remove-repeated-lines-helper inport 
                                                                   outport 
                                                                   line)]
            [else (begin
                    (show line outport)
                    (remove-repeated-lines-helper inport 
                                                  outport 
                                                  line))])))

(define (remove-repeated-lines input-file output-file)
  (let ([inport (open-input-file input-file)]
        [outport (open-output-file output-file)])
    (remove-repeated-lines-helper inport 
                                  outport 
                                  null)
    (close-input-port inport)
    (close-output-port outport)))

;; 22.6  Write a lookup procedure that takes as arguments a filename and a word.
;; The procedure should print (on the screen, not into another file) 
;; only those lines from the input file that include the chosen word. 
; (lookup "r5rs" "to")
(define (lookup-helper inport the-word)
  (let ([line (read-string inport)])
    (display-all "here is line: " line)
    (cond [(eof-object? line) 'done]
          [(number? (srfi-13:string-contains line the-word)) 
           (begin
             (display-all line)
             (lookup-helper inport the-word))]
          [else (lookup-helper inport the-word)])))
  
(define (lookup input-file the-word)
  (let ([inport (open-input-file input-file)])
    (lookup-helper inport the-word)
    (close-input-port inport)))

;; 22.7  Write a page procedure that takes a filename as argument and prints the file a screenful at a time. 
;; Assume that a screen can fit 24 lines; 
;; your procedure should print 23 lines of the file and then a prompt message, 
;; and then wait for the user to enter a (probably empty) line. 
;; It should then print the most recent line from the file again (so that the user will see some overlap between screenfuls) 
;; and 22 more lines, and so on until the file ends. 
(define (page-helper inport prev-line the-num))

(define (page file-name)
  (let ([inport (open-input-file file-name)])
    (page-helper inport null 0)
    (close-input-port file-name)
)  
)

;; delete files we may have created
(delete-our-files '("songs2" "dddbmt-reversed" "results" "r5rs-just"
                    "all-states-we-have" "beatles-output"))

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
  ;; 22.2
  (check-equal? (count-file-lines "a-states") 4)
  (check-equal? (count-file-lines "i-states") 4)
  (check-equal? (count-file-lines "n-states") 8)
  (check-equal? (count-file-lines "r5rs")    10)
  
  ;; 22.3
  (check-equal? (count-file-words "r5rs")   70)
  (check-equal? (count-file-words "grades") 24)

  ;; 22.4
  (check-equal? (count-file-chars "r5rs")   456)
  (check-equal? (count-file-chars "grades") 84)
  

) ;; end module+ test 

