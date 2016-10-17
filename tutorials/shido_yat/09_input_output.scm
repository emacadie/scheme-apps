;; 09 Input and output

;; 1 input
(define (read-file file-name)
    (let ((p (open-input-file file-name)))
            (let loop ((ls1 '()) (c (read-char p)))
                (if (eof-object? c)
                    (begin
                        (close-input-port p)
                        (list->string (reverse ls1)))
                    (loop (cons c ls1) (read-char p))))))
(read-file "./tutorials/shido_yat/hello.txt")

(define (read-file file-name)
    (call-with-input-file file-name
        (lambda (p)
            (let loop ((ls1 '()) (c (read-char p)))
                (if (eof-object? c)
                    (begin
                        (close-input-port p)
                        (list->string (reverse ls1)))
                    (loop (cons c ls1) (read-char p)))))))

(define (read-file file-name)
    (with-input-from-file file-name
        (lambda ()
            (let loop ((ls1 '()) (c (read-char)))
                (if (eof-object? c)
                    (list->string (reverse ls1))
                    (loop (cons c ls1) (read-char)))))))

;; that last one is a bit shorter
(define (s-read file-name)
    (with-input-from-file file-name
        (lambda ()
            (let loop ((ls1 '()) (s (read)))
                (if (eof-object? s)
                    (reverse ls1)
                    (loop (cons s ls1) (read)))))))
(s-read "./tutorials/shido_yat/parens.txt")

;; exercise 1
;; Write the function read-lines that returns a list of strings which correspond to each line of file contents. The newline character is represented by #\Linefeed in Scheme. Following is the result of applying this function to the hello.txt.

;; (read-lines "hello.txt") -> ("Hello world!" "Scheme is an elegant programming language.")
(define (my-read-lines file-name)
    (call-with-input-file file-name
        (lambda (p)
            (let loop ((ls1 '()) (c (read-line p)))
                (if (eof-object? c)
                    (begin
                        (close-input-port p)
                        (reverse ls1))
                    (loop (cons c ls1) (read-line p)))))))
;; this one is easy since R7RS has read-line
;; shido's answer
(define (group-list ls sep)
  (letrec ((iter (lambda (ls0 ls1)
		   (cond
		    ((null? ls0) (list ls1))
		    ((eqv? (car ls0) sep) 
		     (cons ls1 (iter (cdr ls0) '())))
		    (else (iter (cdr ls0) (cons (car ls0) ls1)))))))
    (map reverse (iter ls '()))))


(define (read-lines file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop((ls1 '()) (c (read-char)))
	(if (eof-object? c)
	    (map list->string (group-list (reverse ls1) #\Linefeed))  ; *
	    (loop (cons c ls1) (read-char)))))))
;;; 3 output to files
;; exercise 2
;; Write a function to copy files (my-copy-file).
(define (my-copy-file src-file dest-file)
    (let ((p (open-input-file src-file)))
            (let loop ((ls1 '()) (c (read-char p)))
                (if (eof-object? c)
                    (begin
                        (define outp (open-output-file dest-file))
                        (close-input-port p)
                        (write (list->string (reverse ls1)) outp)
                        (close-output-port outp))
                    (loop (cons c ls1) (read-char p))))))
(my-copy-file "./tutorials/shido_yat/hello.txt" "./tutorials/shido_yat/goodbye.txt")
;; I am printing a list
;; now I get this: "Hello world!\nScheme is an elegant programming language.\n"
;; shido's answer - why didn't I think of that? - but char at a time is pretty slow - try it with a video file
(define (shido-copy-file from to)
  (let ((pfr (open-input-file from))
	(pto (open-output-file to)))
    (let loop((c (read-char pfr)))
      (if (eof-object? c)
	  (begin
	    (close-input-port pfr)
	    (close-output-port pto))
	  (begin
	    (write-char c pto)
	    (loop (read-char pfr)))))))
(shido-copy-file "./tutorials/shido_yat/hello.txt" "./tutorials/shido_yat/goodbye.txt")
;; exercise 3
;; Write the function (print-lines) that takes arbitrary number of strings as arguments and outputs them to the standard output. The output strings should be separated by newline.
(define (my-print-lines . x)
    (display-all "x is " x ", carls is " (car x) ", cdr ls is " (cdr x))
    (let loop ((carls (car x)) (cdrls (cdr x)))
        (if (= 0 (length cdrls)) 
            (begin
                (display carls)
                (newline))
            (begin
                (display carls)
                (newline)
                (loop (car cdrls) (cdr cdrls))))))
(my-print-lines "This" "is" "fun")
;; yes, I like the let loop. So sue me.
;; I am guessing Shido will use letrec? foreach?
;; shido's answer
(define (print-lines . lines)
  (let loop((ls0 lines))
      ;; (display-all "Here is ls0: " ls0)
    (if (pair? ls0)
        (begin
         (display (car ls0))
         (newline)
         (loop (cdr ls0))))))
;; pair? seems to return true for a list that is not empty
;; so it's not really looking at a pair
;; is this the opposite of empty? -- or checking length for 0
(define (print-lines . lines)
  (let loop ((ls0 lines))
      ;; (display-all "Here is ls0: " ls0)
    (if (not (= 0 (length ls0)))
        (begin
         (display (car ls0))
         (newline)
         (loop (cdr ls0))))))


