;; 17 Lazy Evaluation

(define laz (delay (+ 1 2)))
laz ;; 3
(promise? laz) ;; #t
(force laz) ;; 3
(* 10 (force laz)) ;; 30
;; you can call "force" multiple times, meaning you can re-use your promise multiple times
;; 4.1: functions and macro for infinite sequences
;; car for lazy evaluation
(define lazy-car car)

;; cdr for lazy evaluation
(define (lazy-cdr ls)
    (force (cdr ls)))

;; lazy cons
(define-syntax lazy-cons
    (syntax-rules ()
        ((_ a b) (cons a (delay b)))))

;; lazy map
(define (lazy-map fn . lss)
    (if (memq '() lss)
        '()
        (lazy-cons (apply fn (map lazy-car lss))
            (apply lazy-map fn (map lazy-cdr lss)))))

;; lazy filter
(define (lazy-filter pred ls)
    (if (null? ls)
        '()
        (let ((obj (lazy-car ls)))
            (if (pred obj)
                (lazy-cons obj (lazy-filter pred (lazy-cdr ls)))
                (lazy-filter pred (lazy-cdr ls))))))

;; returns nth item of the lazy list
(define (lazy-ref ls n)
    (if (= n 0)
        (lazy-car ls)
        (lazy-ref (lazy-cdr ls) (- n 1))))

;; returns first n items of the list
(define (head ls n)
    (if (= n 0)
        '()
        (cons (lazy-car ls) (head (lazy-cdr ls) (- n 1)))))

;; sequences
;; 4.2. Infinite sequences
;; Infinite sequences are represented concisely using lazy-cons and lazy-map. shido presents two examples:
;; Sequences in which the next term is defined by the previous term such as arithmetic and geometric sequences.
;; Fibonacci series. 
;; infinite sequences represented by a_(n+1) = f(a_n)
(define (inf-seq a0 f)
    (lazy-cons a0 (inf-seq (f a0) f)))

;; arithmatic sequence
(define (ari a0 d)
    (inf-seq a0 (lambda (x) (+ x d))))

;; geometric sequence
(define (geo a0 r)
    (inf-seq a0 (lambda (x) (* x r))))

;; Make two geometric sequences:
;; g1, initial term 1, ratio 2.
;; g2, initial term 1, ratio 1/2. 
;; then evaluate the first 10 terms using the head
(define g1 (head (geo 1 2) 10))
(define g2 (head (geo 1 (/ 1 2)) 10))
;; shido does this:
(define g1 (geo 1 2))
(define g2 (geo 1 (/ 1 2)))
(head g1 10)
(head g2 10)
;; Next, calculate products of terms in g1 and g2 using the lazy-map and evaluated the first 10 terms using the head. You will see a sequence of 1, which indicates that the calculation is done properly. 
(head (lazy-map * g1 g2) 10)

(define ar1 (ari 1 1))
(head ar1 10)
;; Then extract even numbers from the ar1 using the lazy-filter and evaluate first 10 terms using the head
(head (lazy-filter even? ar1) 10)

(define fib
    (lazy-cons 1
        (lazy-cons 1
            (lazy-map + fib (lazy-cdr fib)))))
(head fib 20)

;; 4.3 application of lazy evaluation to numerical calculations
;; newton-raphson method
(define (newton-raphson n)
    (inf-seq 1 (lambda (x) (/ (+ x (/ n x)) 2))))

;; return a reasonable answer 
(define (lazylist->answer ls eps)
    (let ((e1 (- 1.0 eps))
            (e2 (+ 1.0 eps)))
        (let loop ((val (lazy-car ls))
                (ls1 (lazy-cdr ls)))
            (let ((val2 (lazy-car ls)))
                (if (or (zero? val2) (< e1 (/ val val2) e2))
                    (exact->inexact val2)
                    (loop val2 (lazy-cdr ls1)))))))

(define (my-sqrt n eps)
    (lazylist->answer (newton-raphson n) eps))
(my-sqrt 9 0.0000001) ;; 1.0, not what shido got

;; 4.3.2 numerical differentiation
;; primitive function for differentiation
(define (easydiff f x h)
    (/ (- (f (+ x h)) (f x)) h))

;; create a lazy list of approximation for differentiation
(define (lazylist-diff h0 f x)
    (lazy-map (lambda (h) (easydiff f x h)) (geo h0 0.5)))

(define (lazy-second ls)
    (lazy-car (lazy-cdr ls)))

;; eliminate rror from the approximation
;; I think bitwise-arithmetic-shift is correct function 
;; to replace MIT Scheme fix:lsh
(define (elimerror n ls)
    (let ((a (lazy-car ls))
            (b (lazy-second ls))
            (c (bitwise-arithmetic-shift 1 n))) ;; (expt 2 n) 
            (lazy-cons  (/ (- (* b c) a) (- c 1))
                (elimerror n (lazy-cdr ls)))))

(define (elimerror n ls)
    (let ((a (lazy-car ls))
            (b (lazy-second ls))
            (c (expt 2 n))) ;; same result as aboveS
            (lazy-cons  (/ (- (* b c) a) (- c 1))
                (elimerror n (lazy-cdr ls)))))

(define (log2 x)
    (/ (log x) (log 2)))

;; estimate 'n' in elimerror
(define (order ls)
    (let* ((a (lazy-car ls))
            (b (lazy-second ls))
            (c (lazy-ref ls 2))
            (d (- (/ (- a c) (- b c)) 1.0)))
        (cond
            ((< d 2) 1)
            ((<= 2 d 16) (inexact->exact (round (log2 d))))
            (else 4))))

;; improve convergence of lazy list of approximation
(define (improve ls)
    (elimerror (order ls) ls))

;; further improve convergence of the list
(define (super ls)
    (lazy-map lazy-second (inf-seq ls improve)))

;; calculate differentiation of function 'f' at x within error range
;; h0 is initial window width
(define (diff f x h0 eps)
    (lazylist->answer (super (lazylist-diff h0 f x)) eps))

;; try them out
(diff sin 0.0 0.1 0.0000001) ;; 0.9995833854135666, shido got .9999999999999516
(diff exp 0.0 0.1 0.000001) ;; 1.0254219275204823, shido got .9999999991733471

;; integration
;; primitive integration
(define (easyintegrate f a b)
    (* (/ (+ (f a) (f b)) 2) (- b a)))

;; create the lazy list of approximation for integration
(define (lazylist-integrate f a b)
    (let ((mid (/ (+ a b) 2)))
        (lazy-cons (easyintegrate f a b)
            (lazy-map + (lazylist-integrate f a mid) (lazylist-integrate f mid b)))))

;; integrate function f in a range of a and b within error eps
(define (integrate f a b eps)
    (lazylist->answer (super (lazylist-integrate f a b)) eps))

(define pi (* 4 (atan 1)))
(integrate sin 0 pi 0.0000001) ;; 1.5707963267948968, shido got 2.000000002272428
(integrate exp 0 1 0.0000001) ;; 1.7539310924648255, shido got 1.7182818277724858
(- (exp 1) 1) ;; 1.718281828459045



            


