;; more functions for simply scheme
;; scheme complains it can't find vowel?
(define (vowel? letter)
  (member? letter 'aeiou))

(define (display-all . vs)
  (for-each display vs)
  (newline))

(define (trim-leading-zeros the-num)
  (if (not (equal? (first the-num) 0))
      the-num
      (trim-leading-zeros (butfirst the-num))))

