;; I have not figured out how to get different scheme implementations running in a REPL
;; so I will just copy and paste for now
;; JEdit regex to clean up kawa prompt when I copy/paste: .*kawa:\d{1,}\|\#  then \s\n or \#\|kawa:\d{1,}\|\#\s

;; booleans
(boolean? #t)
(boolean? "Hello, World!")
(not #f)
(not #t)
(not "Hello, World!") ;; only true is true. Far out, man.

;; numbers
;; not like Java at all!
;; from tutorial:
;; integer (42) is a rational (22/7) is a real (3.1416) is a complex (2+3i) number is a number
(number? 42) ;; #t
(number? #t) ;; #f
(complex? 2+3i) ;; #t
(complex? 2 + 3i) ;; /dev/stdin:5:1: call to 'complex?' has too many arguments (3; must be 1)
(real? 2+3i) ;; #f
(real? 3.1416) ;; #t
(real? 22/7) ;; #t
(real? 42) ;; #t
(rational? 2+3i) ;; #f
(rational? 3.1416) ;; #f
(rational? 22/7) ;; #t
(integer? 22/7) ;; #f
(integer? 42) ;; #t

(exact->inexact (/ 22 7)) ;; 3.142857142857143

;; general purpose equality
(eqv? 42 42) ;; #t
(eqv? 42 #f) ;; #f
(eqv? 42 42.0) ;; #f

;; numerical equality
(= 42 42) ;; #t
(= 42 #f) ;; #f
(= 42 42.0) ;; #t

;; other comparitors
(< 3 2) ;; #f
(>= 4.5 22/7) ;; #t

;; other operators, some can take multiple args
(+ 1 2 3) ;; 6
(- 5.2 3) ;; 2.2
(- 5.2 22/7) ;; 2.0571428571428574
(- 7 2 1) ; 4
(* 2 3 4) ;; 24
(/ 6 3) ;; 2
(/ 60 5) ;; 12
(/ 60 5 4) ;; 3
(/ 22 7) ;; 22/7
(expt 2 3) ;; 8 - this is taking 2 to the power of 3
(expt 4 1/2) ;; 2.0

(- 4) ;; -4
(/ 4) ;; 1/4

(max 1 3 4 2 3) ;; 4
(min 1 3 4 2 3) ;; 1
(abs 3) ;; 3
(abs -4) ;; 4

;; characters - must be prefixed with #\
(char? #\c) ;; #t
(char? 1) ;; #f
(char? #\;) ;; #t
(char? #\1) ;; #t
;; some chars have descriptive names
(char? #\newline) ;; #t
(char? #\tab) ;; #t
(char? #\space) ;; #t
(char? #\ ) ;; #t

;; comparison
(char=? #\a #\a) ;; #t
(char<? #\a #\b) ;; #t
(char>? #\a #\b) ;; #f
(char-ci=? #\a #\A) ;; #t
(char-ci<? #\a #\B) ;; #t
(char-downcase #\A) ;; a
(char-upcase #\a) ;; A

;; symbols
#t ;; #t
42 ;; 42
#\c ;; c
(quote xyz) ;; xyz
'E ;; E
'xyz ;; xyz
(symbol? 'xyz) ;; #t
(symbol? 42) ;; #f
(symbol (quote jjj)) ;; jjj
(symbol? (quote jjj)) ;; #t
(symbol? "htht") ;; #f
(eqv? 'Calorie 'calorie) ;; #f
(define xyz 9) ;; define a symbol and set initial value
xyz ;; 9
(set! xyz #\c) ;; change the value
xyz ;; c
(set xyz "hello") ;; causes an error

;; compound data types
;; strings
"Hello World" ;; Hello World
(string #\h #\e #\l #\l #\o) ;; hello
(define greeting "Hello")
(define greeting "hello; hello")
greeting ;; hello; hello
(string-ref greeting 0) ;; h
(string-ref greeting 4) ;; o
(string-append "E"
 " Pluribus "
 "Unum") ;; E Pluribus Unum
(define a=3-char-long-string (make-string 3))
a=3-char-long-string
(make-string 3) 
(set! a=3-char-long-string "ll")
a=3-char-long-string ;; ll
(string? "hello") ;; #t
(string? 43) ;; #f
(define hello (string #\H #\e #\l #\l #\o))
hello ;; Hello
(string-set! hello 1 #\a)
hello ;; Hallo

;; vectors - sequances that can contain anything
(vector 0 1 2 3 4) ;; #(0 1 2 3 4)
(define v (make-vector 5)) ;; define a vector named "v" with 5 elements
;; #(0 0 0 0 0)
(vector-set! v 2 "h") ;; results in v containing #(0 0 "h" 0 0)
(vector? v) ;; #t
(vector? "Hello") ;; #f
(vector-ref v 2) ;; "h"

;; dotted pairs and lists
(cons 1 #t) ;; (1 . #t)
'(1 . #t) ;; (1 . #t)
(define x (cons 1 #t))
x ;; (1 . #t)
(car x) ;; 1
(cdr x) ;; #t
(set-car! x 2)
x ;; (2 . #t)
(set-cdr! x "false")
x ;; (2 . "false")
(define y (cons (cons 1 2) 3))
y ;; ((1 . 2) . 3)
(car  (car y)) ;; 1
(cdr (car y)) ;; 2
(caar y) ;; 1
(cdar y) ;; 2
(cons 1 (cons 2 (cons 3 (cons 4 5)))) ;; (1 2 3 4 . 5)
(list 1 2 3 4) ;; (1 2 3 4)
(list 1 2 3 "hello") ;; (1 2 3 "hello")
'(1 2 3 4) ;; (1 2 3 4)
(define yy (list 1 2 3 4)) 
yy ;; (1 2 3 4)
(list-ref yy 0) ;; 1
(list-ref yy 3) ;; 4
(list-tail yy 1) ;; (2 3 4)
(list-tail yy 3) ;; (4)
(pair? '(1 . 2)) ;; #t
(pair? '(1 2)) ;; #t
(pair? '()) ;; #f
(list? '()) ;; #t
(null? '()) ;; #t
(list? '(1 2)) ;; #t
(list? '(1 . 2)) ;; #f
(null? '(1 2)) ;; #f
(null? '(1 . 2)) ;; #f

;; conversions between types
(char->integer #\d) ;; 100
(integer->char 50) ;; #\2
(string->list "hello") ;; (#\h #\e #\l #\l #\o)
(number->string 16) ;; "16"
(string->number "16") ;; 16
(string->number "I love Scheme") ;; #f
(string->number "16" 8) ;; 14
(symbol->string 'symbol) ;; "symbol"

;; other data types
;; converting between types
(char->integer #\d) ;; 100
(integer->char 50) ;; #\2
(string->list "hello") ;; (#\h #\e #\l #\l #\o)
(number->string "16") ;; Exception in number->string: "16" is not a number.
(number->string 16) ;; "16"
(string->number "16") ;; 16
(string->number "I love Scheme") ;; #f
(string->number "16" 8) ;; 14
(symbol->string 'symbol) ;; "symbol"

;; Scheme also has procedures as a data type
symbol->string ;; #<procedure symbol->string>
cons ;; #<procedure cons>
make-list ;; #<procedure make-list>
;; I/O conduits are called "ports" in Scheme.
;; yet another use for word "port"
(display "Hello, World!" (current-output-port)) ;; Hello, World!



