#lang simply-scheme

; Chapter 20 Input and Output

(require (prefix-in ttt: "ttt.rkt"))

(butfirst '(This is Chapter 20 Input and Output))

;; Chapter 20 Input and Output

;; talks about effects vs values
;; introduces the "begin" special form
#|
Quote from the book defining functions and procedures:
Sequencing and side effects are radical departures from the idea of functional programming. 
In fact, we'd like to reserve the name function for something that computes and returns one value, with no side effects. 
"Procedure" is the general term for the thing that lambda returns—an embodiment of an algorithm. 
If the algorithm is the kind that computes and returns a single value without side effects, then we say that the procedure implements a function.
|#

;; There is some stuff about show, display and newline
;; I think show is like display in that it adds a newline at the end
;; You still get a newline from display at the REPL
;; Introduces strings
;; Introduces concept of "higher order procedure", like HOF with functions
;; so "for-each" is like "map" for when you need to print
;; unlike "map" it will not return a new list
;; plus, while the return list of "map" is in the correct order, 
;; it might not execute the elements in the right order
;; for HOP, we might care about order

#|
They say "align" is not part of standard Scheme, so here is their paragraph on it
Align takes three arguments. 
The first is the value to be displayed. 
The second is the width of the column in which it will be displayed; the returned value will be a word with that many characters in it. 
The third argument is the number of digits that should be displayed to the right of the decimal point. 
(If this number is zero, then no decimal point will be displayed.) 
The width must be great enough to include all the digits, as well as the decimal point and minus sign, if any. 
|#

;; When sequencing is important, you can use let


(define (effect x)
  (show x)
  'done)

(define (value x)
  x)

(effect '(oh! darling))

(value '(oh! darling))

; (bf (effect '(oh! darling)))

; (bf (value '(oh! darling)))

(define (lots-of-effect x)
  (effect x)
  (effect x)
  (effect x))

(define (lots-of-value x)
  (value x)
  (value x)
  (value x))

;; some ttt stuff
; from chapter 10 in my "simply.scheme" sub-repo
(define (already-won? position me)
  (not (empty? (keep (lambda (triple) (= (appearances me triple) 3))
         (ttt:find-triples position)))))

(define (tie-game? position)
  (not (member? '_ position)))

;; from the book
(define (stupid-ttt position letter)
  (location '_ position))

(define (location letter word)
  (if (equal? letter (first word))
      1
      (+ 1 (location letter (bf word)))))

(define (add-move square letter position)
  (if (= square 1)
      (word letter (bf position))
      (word (first position) 
            (add-move (- square 1) letter (bf position)))))

(define (play-ttt x-strat o-strat)
  (play-ttt-helper x-strat o-strat '_________ 'x))

(define (play-ttt-helper x-strat o-strat position whose-turn)
  (cond [(already-won? position (ttt:opponent whose-turn))
         (list (ttt:opponent whose-turn) 'wins!)]
        [(tie-game? position) '(tie game)]
        [else (let ((square (if (equal? whose-turn 'x)
                                (x-strat position 'x)
                                (o-strat position 'o))))
                (play-ttt-helper x-strat
                                 o-strat
                                 (add-move square whose-turn position)
                                 (ttt:opponent whose-turn)))]))

(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (read))

(define (print-position position)
  (print-row (subword position 1 3))
  (show "-+-+-")
  (print-row (subword position 4 6))
  (show "-+-+-")
  (print-row (subword position 7 9))
  (newline))

(define (print-row row)
  (maybe-display (first row))
  (display "|")
  (maybe-display (first (bf row)))
  (display "|")
  (maybe-display (last row))
  (newline))

(define (maybe-display letter)
  (if (not (equal? letter '_))
      (display letter)
      (display " ")))

(define (subword wd start end)
  ((repeated bf (- start 1))
   ((repeated bl (- (count wd) end))
    wd)))

;; commenting out first (read-line) works
(define (music-critic)                       ;; second version
  ; (read-line)   ; See explanation below.
  (show "What's your favorite Beatles song?")
  (let ((song (read-line)))
    (show (se "I like" song "too."))))




;; introduces read-line, but there is a read-line in Scheme

(define (square-root-table nums)
  (if (null? nums)
      'done
      (begin (display (align (car nums) 7 1))
	     (show (align (sqrt (car nums)) 10 5))
	     (square-root-table (cdr nums)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 20.3  Define show in terms of newline and display. 
(define (my-show the-arg)
  (display the-arg)
  (newline))

;; 20.4  Write a program that carries on a conversation like the following example. 
;; What the user types is in boldface.
#|
> (converse)
Hello, I'm the computer.  What's your name? Brian Harvey
Hi, Brian.  How are you? I'm fine.
Glad to hear it.
|#

;; Best I can do right now
;; buntine uses nested lets, which works better than sequential
;; But neither his nor pongsh's works in Racket.
;; I might just skip this one
(define (converse)                       ;; second version
  ; (read-line)   ; See explanation below.
  (begin
    (show "What's your favorite Beatles song?")
    (let [(song (read-line))]
      (show (se "I like" song "too.")))
    (show "What is your name?")
    (let [(name (read-line))]
      (show (se "How are you, " name)))))
;; 20.5  Our name-table procedure uses a fixed width for the column 
;; containing the last names of the people in the argument list. 
;; Suppose that instead of liking British-invasion music you are into 
;; late romantic Russian composers:

;> (name-table '((piotr tchaikovsky) (nicolay rimsky-korsakov)
;		(sergei rachmaninov) (modest musorgsky)))
;Alternatively, perhaps you like jazz:
;> (name-table '((bill evans) (paul motian) (scott lefaro)))
;; Modify name-table so that it figures out the longest last name 
;; in its argument list, adds two for spaces, 
;; and uses that number as the width of the first column. 

;; Mine is a bit "wordy", but I wanted to not deal with optional args
;; assume first call is w/name-lngth of 0
(define (longest-lname-helper list-in name-lngth)
  (cond [(empty? list-in) name-lngth]
        ;; (car (cdr (car list-in))) is the first last name of the list
        [(> (count (car (cdr (car list-in)))) name-lngth) 
         (longest-lname-helper (cdr list-in)
                               (count (car (cdr (car list-in)))))]
        [else (longest-lname-helper (cdr list-in) name-lngth)]))

(define (get-longest-lname-length name-list)
  (longest-lname-helper name-list 0))

(define (name-table-helper names longest-lname-len)
  (if (null? names)
      'done
      (begin 
        (display (align (cadar names) longest-lname-len))
        (show (caar names))
        (name-table-helper (cdr names) longest-lname-len))))

(define (name-table names)
  (name-table-helper names 
                     (+ 2 (get-longest-lname-length names))))

(module+ test
  (require rackunit)
  (check-true #t)
  (define (check-three-things-equal? result their-append-rsl my-append-rsl)
  (unless (and (check-equal? result their-append-rsl)
               (check-equal? result my-append-rsl))
    (fail-check)))

  ;; 20.05
  (define jazz-list '((bill evans) 
                      (paul motian) 
                      (scott lefaro)))
  (define russ-composers '((piotr tchaikovsky) 
                           (nicolay rimsky-korsakov) 
                           (sergei rachmaninov) 
                           (modest musorgsky)))

  (check-equal? 6  (get-longest-lname-length jazz-list))
  (check-equal? 15 (get-longest-lname-length russ-composers))

#|
  (check-equal?  )
|#

) ;; end module+ test 

