;; no trace, so let's to this
(define (display-all . vs)
  (for-each display vs)
  (newline))

(define (explode wd)
  (display-all "Calling explode " wd)
  (if (empty? wd)
      '()
      (se (first wd) (explode (bf wd)))))

;; 13.1  Trace the explode procedure from page there and invoke
; (explode 'ape)
; How many recursive calls were there? What were the arguments to each recursive call? Turn in a transcript showing the trace listing. 
(explode 'ape)
   Calling explode ape
   Calling explode pe
   Calling explode e
   Calling explode
   (a p e)

;  13.2  How many pigl-specialist little people are involved in evaluating the following expression?
; (pigl 'throughout)
; What are their arguments and return values, and to whom does each give her result?
(define (pigl wd)
  (display-all "Calling pigl " wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))
;; I think there will be two calls
 Calling pigl throughout
   Calling pigl hroughoutt
   Calling pigl roughoutth
   Calling pigl oughoutthr
   oughoutthray
;; Four. I meant four. Really.

;;  13.3  Here is our first version of downup from Chapter 11. It doesn't work because it has no base case.
(define (downup wd)
  (display-all "calling downup " wd)
  (se wd (downup (bl wd)) wd))
; > (downup 'toe)
; ERROR: Invalid argument to BUTLAST: ""
; Explain what goes wrong to generate that error. In particular, why does Scheme try to take the butlast of an empty word? 
;; You are not checking the length of the word, so it calls "butlast" on an empty string
(downup 'toe)
calling downup toe
calling downup to
calling downup t
calling downup

Error: Invalid argument to BUTLAST: : "\"\""

;;  13.4  Here is a Scheme procedure that never finishes its job:

(define (forever n)
  (display-all "calling forever " n)
  (if (= n 0)
      1
      (+ 1 (forever n))))

; Explain why it doesn't give any result. 
; (If you try to trace it, make sure you know how to make your version of Scheme stop what it's doing and give you another prompt.)
;; It never changes n, it just keeps incrementing it, and n never gets to 0.
;; I stopped it after about 50,000 lines.




