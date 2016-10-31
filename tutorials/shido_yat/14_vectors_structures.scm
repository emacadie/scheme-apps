;; 14 - vectors and structures

;; vectors represeneted by #() which is for anonymous functions in Clojure
;; #(1 2 3)

(require 'list-lib) ;; needed for Kawa
(require 'srfi-1) ;; another way for Kawa
(use-modules (srfi srfi-1)) ;; for Guile
;; shido's function to calculate sum of vectors
(define (vector-add v1 v2)
    (let ((lenv1 (vector-length v1))
            (lenv2 (vector-length v2)))
        (if (= lenv1 lenv2)
            (let ((v (make-vector lenv1)))
                (let loop ((i 0))
                    (if (= i lenv1)
                        v
                        (begin
                            (vector-set! v i (+ (vector-ref v1 i) (vector-ref v2 i)))
                            (loop (+ 1 i))))))
            (error "different dimensions"))))

(vector-add #(1 2 3) #(4 5 6)) ;; 
#(5 7 9)

;; Exercise 1
;; Write a function that calculates the inner product of two vectors. 
;; like map, or reduce?
;; turns out it's both
(define (inner-product vec1 vec2)
       (let ((len1 (vector-length vec1))
     	(len2 (vector-length vec2)))
         (if (= len1 len2)
     	(let loop ((i 0) (pro 0))
     	  (if (= i len1)
     	      pro
     	      (loop (+ 1 i) (+ pro
     			      (* (vector-ref vec1 i)
     				 (vector-ref vec2 i))))))
     	(error "different dimensions."))))

(inner-product  #(1 2 3) #(4 5 6)) ;; 32

(reduce + 0 (map * #(1 2 3) #(4 5 6)))

;; 3 structures
;;; simple structure definition, from next section does not work in Kawa or guile

;; it seems we are hitting the limits of usefulness of this tutorial
;; it's a good tutorial, but a bit old (some of the links do not work)
;; and some parts are implementation-specific
;; for all its warts, Common Lisp is looking pretty good

;;; simple structure definition from section 15
;; works in Kawa, not in Guile

;;; lists of symbols -> string
(define (append-symbol . ls)
  (let loop ((ls (cdr ls)) (str (symbol->string (car ls))))
    (if (null? ls)
	str
	(loop (cdr ls) (string-append str "-" (symbol->string (car ls)))))))

;;; obj -> ls -> integer
;;; returns position of obj in ls
(define (position obj ls)
  (letrec ((iter (lambda (i ls)
		   (cond
		    ((null? ls) #f)
		    ((eq? obj (car ls)) i)
		    (else (iter (+ 1 i) (cdr ls)))))))
    (iter 0 ls)))

;;; list -> integer -> list
;;; enumerate list items
(define (slot-enumerate ls i)
  (if (null? ls)
      '()
    (cons `((,(car ls)) ,i) (slot-enumerate (cdr ls) (+ 1 i)))))

;;; define simple structure 
(define-syntax defstruct
   (lambda (expr)
     (let ((struct (second expr))
           (slots  (map (lambda (x) (if (pair? x) (car x) x)) (cddr expr)))
	   (veclen (- (length expr) 1)))
	   
       `(begin   
	  (define ,(string->symbol (append-symbol 'make struct))   ; making instance
	    (lambda ls
              (let ((vec (vector ',struct ,@(map (lambda (x) (if (pair? x) (second x) #f)) (cddr expr)))))
		(let loop ((ls ls))
		  (if (null? ls)
		      vec
		      (begin
                       (vector-set! vec (case (first ls) ,@(slot-enumerate slots 1)) (second ls))
			(loop (cddr ls))))))))

	  (define ,(string->symbol (string-append (symbol->string struct) "?"))  ; predicate
	    (lambda (obj)
	      (and
	       (vector? obj)
	       (eq? (vector-ref obj 0) ',struct))))

	  ,@(map
	     (lambda (slot)
	       (let ((p (+ 1 (position slot slots))))
		 `(begin
		    (define ,(string->symbol (append-symbol struct slot))    ; accessors
		      (lambda (vec)
			(vector-ref vec ,p)))

		    (define-syntax ,(string->symbol                           ; modifier
				     (string-append
				      (append-symbol 'set struct slot) "!"))
		      (syntax-rules ()
			((_ s v) (vector-set! s ,p v)))))))
	     slots)

	  (define ,(string->symbol (append-symbol 'copy struct))      ; copier
	    (lambda (vec)
	      (let ((vec1 (make-vector ,veclen)))
		(let loop ((i 0))
		  (if (= i ,veclen)
		      vec1
		      (begin
			(vector-set! vec1 i (vector-ref vec i))
			(loop (+ 1 i))))))))))))

(defstruct book title authors publisher year isbn)
(define mon-month 
  (make-book 'title  
	     "The Mythical Man-Month: Essays on Software Engineering"
	     'authors
	     "F.Brooks"
	     'publisher
	     "Addison-Wesley"
	     'year
	     1995
	     'isbn))

mon-month ;; #(book The Mythical Man-Month: Essays on Software Engineering F.Brooks
  Addison-Wesley 1995 201835959)
(define qq (copy-book mon-month))
qq ;; #(book The Mythical Man-Month: Essays on Software Engineering F.Brooks Addison-Wesley 1995 201835959)

(book-title qq) ;; The Mythical Man-Month: Essays on Software Engineering
(book-year qq) ;; 1995
(set-book-year! qq 2000)
(book-year qq) ;; 2000
qq ;; #(book The Mythical Man-Month: Essays on Software Engineering F.Brooks Addison-Wesley 2000 201835959)

;; also look at srfi 9
(require 'srfi-9) 
(import (srfi 9)) ;; actually not needed
;; example from https://www.gnu.org/software/guile/manual/html_node/SRFI_002d9-Records.html#SRFI_002d9-Records
(define-record-type <employee>
  (make-employee name age salary)
  employee?
  (name    employee-name)
  (age     employee-age    set-employee-age!)
  (salary  employee-salary set-employee-salary!))
 
(define fred (make-employee "Fred" 45 20000.00))
(employee-age fred) ;; 45
(set-employee-age! fred 44)
(employee-age fred) ;; 44
;; so no display for srfi 9

;; do we need greater-than and less-than signs?
(define-record-type drone
  (make-drone name age salary)
  drone?
  (name    drone-name)
  (age     drone-age    set-drone-age!)
  (salary  drone-salary set-drone-salary!))

(define-record-type drone
  (make-drone name age salary)
  drone?
  (name    drone-name)
  (age     drone-age    set-drone-age!)
  (salary  drone-salary set-drone-salary!))

(define office-guy (make-drone "Steve C" 34 2000))
(drone-age office-guy) ;; 34
(set-drone-age! office-guy 44)
(drone-age office-guy) ;; 44

(define-record-type pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))
(define r (kons '3 '4))
r ;; atInteractiveLevel-11$pare@6c3708b3
;; so no handy display
(kdr r) ;; 4
(kar r) ;; 3
(set-kar! r '5)
(kar r) ;; 5
(set-kdr! r '6) ;; warning - no declaration seen for set-kdr! unbound location: set-kdr!
;; so you can make a field immutable


