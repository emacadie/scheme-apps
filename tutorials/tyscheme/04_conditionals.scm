;; 4 - Conditionals
(define p 80)
(if (> p 70)
      'safe
      'unsafe)
;; result: safe
(if (< p 90)
      'low-pressure) ;; no 'else' branch
;; result: low-pressure

;; when and unless
(define pressure-tube 55)
(when (< pressure-tube 60)
    (display "'open-valve ")
    (display "'attach-floor-pump-tube ")
    (display "'depress-floor-pump-5 ")
    (display "'detach-floor-pump-tube ")
    (display "'close-valve-tube ")
    (newline))
;; displays:
;; 'open-valve 'attach-floor-pump-tube 'depress-floor-pump-5 'detach-floor-pump-tube 'close-valve-tube

;; equivalent to
(if (< pressure-tube 60)
      (begin
        (display "'open-valve ")
        (display "'attach-floor-pump-tube ")
        (display "'depress-floor-pump-5 ")
        (display "'detach-floor-pump-tube ")
        (display "'close-valve-tube ")
        (newline)))
;; displays: 'open-valve 'attach-floor-pump-tube 'depress-floor-pump-5 'detach-floor-pump-tube 'close-valve-tube 
;; the "if" requires a "begin"
;; equivalent to
(unless (>= pressure-tube 60)
    (display "'open-valve ")
    (display "'attach-floor-pump-tube ")
    (display "'depress-floor-pump-5 ")
    (display "'detach-floor-pump-tube ")
    (display "'close-valve-tube ")
    (newline))
;; displays: 'open-valve 'attach-floor-pump-tube 'depress-floor-pump-5 'detach-floor-pump-tube 'close-valve-tube 

;; cond - multi-branch conditional
;; here is a nested if
(define c #\b)
(if (char<? c #\c) -1
      (if (char=? c #\c) 0
          1))
;; displays: -1
;; same as:
(cond ((char<? c #\c) -1)
    ((char=? c #\c) 0)
    (else 1))
;; displays: -1

;; case
(case c
    ((#\a) 1)
    ((#\b) 2)
    ((#\c) 3)
    (else 4))
;; displays: 2

;; logic "and" and "or" ("not" was covered in section 2)
(and 1 2) ;; 2
(and #f 4) ;; #f
(or 1 2) ;; 1
(or #f 4) ;; 4
(or #f (char? 4)) ;; #f
(define expression-guaranteed-to-cause-error #f)
(and 1 #f expression-guaranteed-to-cause-error) ;; #f
(or 1 #f expression-guaranteed-to-cause-error) ;; 1




