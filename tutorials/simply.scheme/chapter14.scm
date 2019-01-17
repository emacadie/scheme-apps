;; chapter 14
;; display-all is in more-functions.scm 
;; load it before this file

;; using recursion to do "every"
;; From the text:
;; The pattern here is pretty clear. 
;; Our recursive case will do something straightforward to the first of the sentence, 
;; such as squareing it or pigling it, 
;; and we'll combine that with the result of a recursive call on the butfirst of the sentence. 
(define (square-sent sent)
  (display-all "calling square-sent with " sent)
  (if (empty? sent)
      '()
      (se (square (first sent))
          (square-sent (bf sent)))))

;; Now make it tail-recursive.
;; They love to start everything with a sentence
(define (square-sentr sent outp)
  (display-all "calling square-sentr with sent: " sent ", outp: " outp)
  (if (empty? sent)
      outp ;; base case returns output
      (square-sentr (bf sent) (se outp (square (first sent))))))

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pigl-sent sent)
  (display-all "calling pigl-sent with sent: " sent)
  (if (empty? sent)
      '()
      (se (pigl (first sent))
          (pigl-sent (bf sent)))))

;; tail recursive
(define (pigl-sentr sent outp)
  (display-all "calling pigl-sentr with sent: " sent ", outp: " outp)
  (if (empty? sent)
      outp
      (pigl-sentr (bf sent) (se outp(pigl (first sent))))))

(define (disjoint-pairs wd)
  (display-all "calling disjoint-pairs with wd: " wd)
  (cond ((empty? wd) '())
	((= (count wd) 1) (se wd))
	(else (se (word (first wd) (first (bf wd)))
		  (disjoint-pairs (bf (bf wd)))))))

(define (disjoint-pairs-r wd outp)
  (cond ((empty? wd) outp)
	((= (count wd) 1) (se wd))
	(else (se (word (first wd) (first (bf wd)))
		  (disjoint-pairsr (bf (bf wd)))))))


