;; 07 IO

;; files are called "ports" in Scheme

;; writing "hello world" to a file
(define o (open-output-file "greeting.txt"))
(display "hello" o)
(write-char #\space o)
(display 'world o)
(newline o)
(close-output-port o)

;; reading "hello world" from a file
(define i (open-input-file "greeting.txt"))
(read-char i)
(define j (read i))

;; call-with-input-file takes a filename and a procedure
(call-with-input-file "greeting.txt"
    (lambda (i)
        (let* ((a (read-char i))
                (b (read-char i))
                (c (read-char i)))
            (list a b c))))

;; strings are also ports
(define i (open-input-string "hello world"))
(read-char i)
(read i)

(define o (open-output-string))
(write 'hello o)
(write-char #\, o)
(display " " o)
(display "world" o)
(get-output-string o)

;; add2 is defined in 03_forms.scm
;; so in repl started in top dir in repo you could do this:
;; (load-relative "tutorials/tyscheme/03_forms.scm")
;; then call "add2" in repl
;; load-relative in kawa, chicken, not in chez


