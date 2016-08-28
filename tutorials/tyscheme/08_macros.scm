;; 08 Macros

;; define-macro works in kawa, not chez
;; not too clear at the moment what the dot is for in the lambda
;; in section 3:  
;; anything before dot bound to corresponding arg, var after dot picks up everything after that as one list
(define-macro when2
    (lambda (test . branch)
        (list 'if test
            (cons 'begin branch))))

(define pressure-tube 55)
(when2 (< pressure-tube 60)
    (display "'open-valve ")
    (display "'attach-floor-pump-tube ")
    (display "'depress-floor-pump-5 ")
    (display "'detach-floor-pump-tube ")
    (display "'close-valve-tube ")
    (newline))

(define-macro unless2
    (lambda (test . branch)
        (list 'if
            (list 'not test)
            (cons 'begin branch))))
;; or this
(define-macro unless3
    (lambda (test . branch)
        (cons 'when2
            (cons (list 'not test) branch))))
;; so a macro can use another macro

;; macro as a template
;; backtick is template for a list
;; elements are verbatim, except when prefixed bya comma, or comma-splice ,@
;; comma and comma-splice insert macro arguments
;; comma-splice removes outermost set of parentheses (so comma-splice must give a list)
(define-macro when3
    (lambda (test . branch)
        `(IF ,test
            (BEGIN ,@branch))))

(define-macro my-or
    (lambda (x y)
        `(if ,x ,x ,y)))

;; this will print "doing first argument" twice ; in macro, x is evaluated twice
(my-or
  (begin 
    (display "doing first argument")
     (newline)
     #t)
  2)

;; so do this
(define-macro my-or
    (lambda (x y)
        `(let ((temp ,x))
            (if temp temp ,y))))

;; when if we have an arg called "temp"?
(define temp 3)
(my-or #f temp) ;; returns false

;; you could put a plus sign in front of them, because it's rare
;; and hope you don't get unlucky
(define-macro my-or
    (lambda (x y)
        `(let ((+temp ,x))
            (if +temp +temp ,y))))

;; you could also use gensym
;; kawa uses gemtemp
(define-macro my-or
    (lambda (x y)
        (let ((temp (gentemp)))
            `(let ((,temp ,x))
                (if ,temp ,temp ,y)))))

;; I will get back to the fluid-let some other time



