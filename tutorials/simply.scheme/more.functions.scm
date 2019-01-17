;; more functions for simply scheme
;; scheme complains it can't find vowel?
(define (vowel? letter)
  (member? letter 'aeiou))

(define (display-all . vs)
  (for-each display vs)
  (newline))

