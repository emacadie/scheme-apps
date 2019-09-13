#lang simply-scheme

; Chapter 22 Files

(require (prefix-in more: "more-simply.rkt"))
(require (prefix-in srfi-13: srfi/13))

;; Chapter 22 Files Input and Output
(butfirst '(this is chapter 22: i/o))
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
;; Maybe using an SRFI is cheating, but I can live with myself.
; (lookup "r5rs" "to")
(define (lookup-helper inport the-word)
  (let ([line (read-string inport)])
    (more:display-all "here is line: " line)
    (cond [(eof-object? line) 'done]
          [(number? (srfi-13:string-contains line the-word)) 
           (begin
             (more:display-all line)
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
; (page "/home/ericm/Downloads/Sexp.txt")
;; buntine and mengsince decremented, which eliminates a cond clause
;; I should have seen that: That fixes the issue of checking for 22 AND 23
(define (page-helper inport prev-line line-count screen-count)
  ; (display-all "in page-helper, with prev-line: " prev-line ", line-count: " line-count ", screen-count: " screen-count)
  (let ([line (read-string inport)])
    ; (display-all "Here is line: " line)
    (cond [(eof-object? line) 'done]
          [(and (equal? line-count 1) (> screen-count 1))
           (begin
             (more:display-all prev-line)
             (more:display-all line)
             (page-helper inport line (+ 1 line-count) screen-count))]
          [(or (and (equal? 1 screen-count) (< line-count 23))
               (and (> screen-count 1)      (< line-count 22)))
           (begin
             (more:display-all line)
             (page-helper inport line (+ 1 line-count) screen-count))]
          [(or (and (equal? 1 screen-count) (equal? 23 line-count))
               (and (> screen-count 1)      (equal? 22 line-count)))
           (begin
             (more:display-all line)
             (more:display-all "hit <Enter> to proceed")
             (read-line)
             (page-helper inport line 1 (+ 1 screen-count)))]
          [else (more:display-all "In the else")])))

(define (page file-name)
  (let ([inport (open-input-file file-name)])
    (page-helper inport null 1 1)
    (close-input-port inport)))

#|
 22.8  A common operation in a database program is to join two databases, that is, to create a new database combining the information from the two given ones. There has to be some piece of information in common between the two databases. For example, suppose we have a class roster database in which each record includes a student's name, student ID number, and computer account name, like this:

((john alec entwistle) 04397 john)
((keith moon) 09382 kmoon)
((peter townshend) 10428 pete)
((roger daltrey) 01025 roger)

We also have a grade database in which each student's grades are stored according to computer account name:

(john 87 90 76 68 95)
(kmoon 80 88 95 77 89)
(pete 100 92 80 65 72)
(roger 85 96 83 62 74)

We want to create a combined database like this:

((john alec entwistle) 04397 john 87 90 76 68 95)
((keith moon) 09382 kmoon 80 88 95 77 89)
((peter townshend) 10428 pete 100 92 80 65 72)
((roger daltrey) 01025 roger 85 96 83 62 74)

in which the information from the roster and grade databases has been combined for each account name.

Write a program join that takes five arguments: 
two input filenames, two numbers indicating the position of the item within each record that should overlap between the files, and an output filename. For our example, we'd say

> (join "class-roster" "grades" 3 1 "combined-file")

In our example, both files are in alphabetical order of computer account name, 
the account name is a word, 
and the same account name never appears more than once in each file. 
In general, you may assume that these conditions hold for the item that the two files have in common. 
Your program should not assume that every item in one file also appears in the other. 
A line should be written in the output file only for the items that do appear in both files. 
|# 
;; (join-files "who.names" "who.grades" 3 1 "who.output")
;; I think this is better:
;; (join-files "who.names" 3 "who.grades" 1 "who.output")
;; read works for names

;; should after go to more-simply?
(define (after? a b)
  (and (not (before? a b))
       (not (equal? a b))))

(define (join-files-helper inport-a num-a line-a inport-b num-b line-b outport)
  (cond [(or (eof-object? line-a) (eof-object? line-b)) 'done] 
    [(after? (item num-a line-a)
             (item num-b line-b))
         (join-files-helper inport-a num-a line-a inport-b num-b (read inport-b) outport)]
        [(before? (item num-a line-a)
                  (item num-b line-b))
         (join-files-helper inport-a num-a (read inport-a) inport-b num-b line-b outport)]
        [else
         (begin
           (show (append (all-se-item-but-num num-a line-a)
                         (list (item num-a line-a))
                         (all-se-item-but-num num-b line-b)) outport)
           (join-files-helper inport-a num-a (read inport-a) inport-b num-b (read inport-b) outport))]))

;; This mostly works. There are too many parens, but at this point, I don't care.
;; I just want this over with. I am tired of worrying about read, read-line, read-this, read-that, is it a sentence, is it a list.
;; Maybe I will look at buntine's or mengsince1986's solutions later.
;; But yes, I am punting, and I am okay with that.
;; 2019-09-13: I did look at mengsince's, and it inspired me to try again
;; meng calls another function to finalize the list
;; now I am getting one fewer parens
;; The leading 0's on student ids are getting removed
;; but that also happened with meng's and buntine's
;; buntine wrote lines without outermost parens
(define (join-files first-file first-num scnd-file scnd-num outfile)
  (let ([inport-a (open-input-file first-file)]
        [inport-b (open-input-file scnd-file)]
        [outport  (open-output-file outfile)])
    (join-files-helper inport-a first-num (read inport-a) inport-b scnd-num (read inport-b) outport)
    (close-input-port inport-a)
    (close-input-port inport-b)
    (close-output-port outport)))

(define (look-at-who-names)
  (let ([inport (open-input-file "who.names")])
    (who-names-lines inport)
    (close-input-port inport)))

(define (all-se-but-num-helper num the-sent count outp)
  (cond [(empty? the-sent) outp]
        [(equal? num count) (all-se-but-num-helper num 
                                                   (butfirst the-sent) 
                                                   (+ 1 count) 
                                                   outp)]
        [else (all-se-but-num-helper num 
                                     (butfirst the-sent) 
                                     (+ 1 count) 
                                     (se outp (first the-sent)))]))

(define (all-se-item-but-num num the-sent)
  (cond [(> num (count the-sent)) the-sent]
        [(< num 1) the-sent]
        [(equal? num 1) (butfirst the-sent)]
        [(equal? num (count the-sent)) (butlast the-sent)]
        [else (all-se-but-num-helper num the-sent 1 '())]))

;; read works for grades as sentences
;; read-line no good
(define (who-names-lines inport)
  (more:display-all "---------------------------------")
  (let ([line (read inport)])
    (cond [(eof-object? line) 'done]
          [else (begin
                  (more:display-all "here is line: " line
                                    ", here is (first line): " (first line))
                  (more:display-all "sentence of first: " (se (first line)))
                  (more:display-all "Is it a sentence? " (sentence? line) ", is it a list: " (list? line))
                  (more:display-all "here is it as a sentence: " 
                                    (se (first line) ; (list-ref line 0)
                                        (list-ref line 1)
                                        (list-ref line 2)) 
                                    ", here is bl line: " (bl line))
                  (more:display-all "here is the trick: bl then last: " 
                                    (se (bl line)))
                  ; (display-all "Car of the line: " (car line))
                  (who-names-lines inport))])))

(define (delete-if-exists file-name)
  (when (file-exists? file-name)
    (delete-file file-name)))

(define (delete-our-files list-of-files)
  (for-each delete-if-exists list-of-files))

;; delete files we may have created
(delete-our-files '("songs2" "dddbmt-reversed" "results" "r5rs-just"
                    "all-states-we-have" "beatles-output" "Sexp.txt"
                    "who.output"))


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
  
  ;; 22.8
  (define test-sent '(one two three four five))
  (check-equal? (all-se-item-but-num 1 test-sent)
                '(two three four five))
  (check-equal? (all-se-item-but-num 5 test-sent)
                '(one two three four))
  (check-equal? (all-se-item-but-num 6 test-sent)
                test-sent)
  (check-equal? (all-se-item-but-num 0 test-sent)
                test-sent)
  (check-equal? (all-se-item-but-num 2 test-sent)
                '(one three four five))
  (check-equal? (all-se-item-but-num 3 test-sent)
                '(one two four five))
  (check-equal? (after? 'joe 'jod)   #t)
  (check-equal? (after? "joe" "jod") #t)
  (check-equal? (after? 'jod 'joe)   #f)
  (check-equal? (after? "jod" "joe") #f)
  (check-equal? (after? 'joe 'joe)   #f)
  (check-equal? (after? "joe" "joe") #f)
  


) ;; end module+ test 

