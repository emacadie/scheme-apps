#lang simply-scheme

(require (prefix-in more: "more-simply.rkt"))

(butfirst '(trying meng and buntine))

;; my invocation:
;; (join-files "who.names" 3 "who.grades" 1 "who.output")
;; (join-meng "who.names" "who.grades" 3 1 "who.output")

;; from https://github.com/mengsince1986/Simply-Scheme-exercises/blob/master/SS%20Exercises/Exercises%2022.1-22.8.scm

;; (join-meng "who.names" "who.grades" 3 1 "who.output")
(define (join-meng file-a file-b overlap-index-a overlap-index-b file-ab)
  (let ((inport-a (open-input-file file-a))
        (inport-b (open-input-file file-b))
        (outport-ab (open-output-file file-ab)))
    (join-helper inport-a (read inport-a) inport-b (read inport-b) overlap-index-a overlap-index-b outport-ab)
    (close-output-port outport-ab)
    (close-input-port inport-a)
    (close-input-port inport-b)
    'done))

(define (join-helper inport-a data-a inport-b data-b overlap-index-a overlap-index-b outport-ab)
  ; (more:display-all "in join-helper, data-a: " data-a ", index-a: " overlap-index-a ", data-b: " data-b ", index-b: " overlap-index-b)
  
  (if (or (eof-object? data-a) (eof-object? data-b))
      #f
      (let ((overlap-a (list-ref data-a (- overlap-index-a 1)))
            (overlap-b (list-ref data-b (- overlap-index-b 1))))
        (if (before? overlap-a overlap-b)
            (join-helper inport-a (read inport-a) inport-b data-b overlap-index-a overlap-index-b outport-ab)
            (if (equal? overlap-a overlap-b)
                (begin (show (combine-overlap-lsts data-a data-b overlap-b) outport-ab)
                       (join-helper inport-a (read inport-a) inport-b (read inport-b) overlap-index-a overlap-index-b outport-ab))
                (join-helper inport-a data-a inport-b (read inport-b) overlap-index-a overlap-index-b outport-ab))))))

(define (combine-overlap-lsts lst-a lst-b overlap-b)
  (append lst-a (cut-element overlap-b lst-b)))

(define (cut-element target lst)
  (cond ((null? lst) '())
        ((equal? target (car lst)) (cdr lst))
        (else (cons (car lst) (cut-element target (cdr lst))))))

;; https://github.com/buntine/Simply-Scheme-Exercises/blob/master/22-files/22-8.scm
;; (join-buntine "who.names" "who.grades" 3 1 "who.output")
;;
(define (join-buntine infile-a infile-b pos-a pos-b outfile)
  (let ((data-a (read-file infile-a))
        (data-b (read-file infile-b)))
    (write-file (merge-by-field data-a data-b pos-a pos-b)
                outfile)))

; Reads each line of an input port into a list.
(define (read-file path)
  (let ((inport (open-input-file path)))
    (define lines (read-file-helper inport '()))
    (close-input-port inport)
    lines))

(define (read-file-helper inport lines)
  (let ((line (read inport)))
    (if (eof-object? line)
      lines
      (read-file-helper inport
                        (endcons line lines)))))

; Writes each item of lines into an output port.
(define (write-file lines path)
  (let ((outport (open-output-file path)))
    (for-each (lambda (line) (show-line line outport))
              lines)
    (close-output-port outport)
    'done))

; Merges two lists by a matching item denoted by pos-a and pos-b.
(define (merge-by-field data-a data-b pos-a pos-b)
  (if (or (null? data-a) (null? data-b))
    '()
    (merge-by-field-helper data-a data-b pos-a pos-b '())))

(define (merge-by-field-helper data-a data-b pos-a pos-b lst)
  (if (null? data-a)
    lst
    (let ((match (find-by-field data-b
                               pos-b
                               (item pos-a (car data-a)))))
      (if match
        (merge-by-field-helper (cdr data-a)
                               data-b pos-a pos-b
                               (endcons (join-lists-by-field
                                          (car data-a)
                                          match
                                          (item pos-a (car data-a)))
                                        lst))
        (merge-by-field-helper (cdr data-a)
                               data-b pos-a pos-b
                               lst)))))

; Searches a list for a match at the correct position.
; Returns the list element if found, otherwise #f.
(define (find-by-field lst pos value)
  (cond ((null? lst) #f)
        ((equal? (item pos (car lst)) value) (car lst))
        (else (find-by-field (cdr lst) pos value))))

; Joins two lists by a matching field.
; The field at pos in list b will be removed.
(define (join-lists-by-field a b value)
  (cond ((null? b) a)
        ((equal? (car b) value)
          (join-lists-by-field a (cdr b) value))
        (else
          (join-lists-by-field (endcons (car b) a)
                              (cdr b)
                              value))))
    
; Adds an item onto the end of a list
(define (endcons n lst)
  (append lst
          (cons n '())))

(module+ test
  (require rackunit)
  (check-true #t))



