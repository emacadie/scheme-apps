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



