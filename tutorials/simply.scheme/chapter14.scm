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

(define (disjoint-pairsr wd outp)
  (display-all "calling disjoint-pairsr with wd: " wd ", and outp: " outp)
  (cond ((empty? wd) (reverse outp) )
	((= (count wd) 1) (se (reverse outp) wd))
	(else (disjoint-pairsr (bf (bf wd)) (se (word (first wd) (first (bf wd))) outp)))))
;; tail recursive
(define (disjoint-pairsr2 wd outp)
  (display-all "calling disjoint-pairsr with wd: " wd ", and outp: " outp)
  (cond ((empty? wd) outp)
	((= (count wd) 1) (se outp wd))
	(else (disjoint-pairsr2 (bf (bf wd)) (se outp (word (first wd) (first (bf wd))))))))

;; The "keep" pattern - aka filter
;; In every, we perform a function on an element
;; In keep, we decide if we want to keep an element
;; If we do, we just keep it, we do not transform it
(define (keep-three-letter-words sent)
  (display-all "calling keep-three-letter-words with sent: " sent)
  (cond ((empty? sent) '())
        ((= (count (first sent)) 3)
         (se (first sent) (keep-three-letter-words (bf sent))))
        (else (keep-three-letter-words (bf sent)))))

;; Is this tail-recursive? There are two calls
;; Maybe filter needs two calls (but that is another question)
;; Every: one base case, one recursive case
;; Keep: one base case, two (or more?) recursive cases
(define (keep-three-letter-words-r sent outp)
  (display-all "keep-three-letter-words-r with sent: " sent ", outp: " outp)
  (cond ((empty? sent) outp)
        ((= (count (first sent)) 3)
         (keep-three-letter-words-r (bf sent) (se outp (first sent))))
        (else (keep-three-letter-words-r (bf sent) outp))))

(define (keep-vowels wd)
  (cond ((empty? wd) "")
        ((vowel? (first wd))
         (word (first wd) (keep-vowels (bf wd))))
        (else (keep-vowels (bf wd)))))
;; gotta call like this: (keep-vowels-r 'napolean '"")
;; for others, we could use '() for outp
(define (keep-vowels-r wd outp)
  (display-all "calling keep-vowels-r wd: " wd ", outp:" outp)
  (cond ((empty? wd) outp)
        ((vowel? (first wd))
         (keep-vowels-r (bf wd) (word outp (first wd))))
        (else (keep-vowels-r (bf wd) outp))))

;; again, their counter-example is when we look at more than one element at a time
(define (doubles wd)
  (cond ((= (count wd) 1) "")
        ((equal? (first wd) (first (bf wd)))
         (word (first wd) (first (bf wd)) (doubles (bf (bf wd)))))
        (else (doubles (bf wd)))))

(define (doubles-r wd outp)
  (display-all "doubles-r, wd: " wd ", outp: " outp)
  (cond ((= (count wd) 1) outp)
        ((equal? (first wd) (first (bf wd)))
         (doubles-r (bf (bf wd)) (word outp (first wd ) (first (bf wd)))))
        (else (doubles-r (bf wd) outp))))

;; accumulate, aka reduce
(define (addup nums)
  (display-all "calling addup, nums: " nums)
  (if (empty? nums)
      0
      (+ (first nums) (addup (bf nums)))))

(define (addup-r nums outp)
  (display-all "addup-r with nums: " nums ", outp: " outp)
  (if (empty? nums)
      outp
      (addup-r (bf nums) (+ (first nums) outp))))

(define (scrunch-words sent)
  (if (empty? sent)
      ""
      (word (first sent) (scrunch-words (bf sent)))))

(define (scrunch-words-r sent outp)
  (display-all "calling scrunch-words-r, sent: " sent ", outp: " outp)
  (if (empty? sent)
      outp
      ; (word (first sent) (scrunch-words (bf sent)))
      (scrunch-words-r (bf sent) (word outp (first sent)))))

;; From the text:
;; What's the pattern? 
;; We're using some combiner (+ or word) to connect the word we're up to with the result of the recursive call. 
;; The base case tests for an empty argument, 
;; but the base case return value must be the identity element of the combiner function.

;; another one:
; This is a bit harder, since "max" can take multiple args
(define (sent-max sent)
  (if (= (count sent) 1)
      (first sent)
      (max (first sent)
	   (sent-max (bf sent)))))
;; try later
(define (sent-max-r sent outp)
  (if (= (count sent) 1)
      (first sent)
      ; (max (first sent) (sent-max (bf sent)))
      (sent-max-r )))

;; combining patterns
(define (add-numbers sent)
  (cond ((empty? sent) 0)
	    ((number? (first sent))
	     (+ (first sent) (add-numbers (bf sent))))
	    (else (add-numbers (bf sent)))))

;; set outp to 0
;; this might need a "wrapper" function
(define (add-numbers-r sent outp)
  (display-all "add-numbers-r with sent:" sent ", outp: " outp )
  (cond ((empty? sent) outp)
	    ((number? (first sent))
	     (add-numbers-r (bf sent) (+ (first sent) outp)))
	    (else (add-numbers-r (bf sent) outp))))

(add-numbers '(if 6 were 9))

(accumulate + (keep number? '(if 6 were 9)))

; another one with keep and every
(define (has-vowel? wd)
  (not (empty? (keep-vowels wd))))
;; one issue: every word has a vowel
(define (safe-pigl sent)
  (cond ((empty? sent) '())
	((has-vowel? (first sent))
	 (se (pigl (first sent)) (safe-pigl (bf sent))))
	(else (safe-pigl (bf sent)))))

(every pigl (keep has-vowel? '(my pet fly is named xyzzy)))

(safe-pigl '(my pet fly is named xyzzy))
;; I may skip this one

;; all three
(define (acronym sent)
  (cond ((empty? sent) "")
	((real-word? (first sent))
	 (word (first (first sent))
	       (acronym (bf sent))))
	(else (acronym (bf sent)))))

;; call like this: (acronym-r '(this is a sentence) "")
(define (acronym-r sent outp)
  (display-all "acronym-r with sent: " sent ", outp: " outp)
  (cond ((empty? sent) outp)
	    ((real-word? (first sent))
         (acronym-r (bf sent) (word outp (first (first sent)))))
	    (else (acronym-r (bf sent) outp))))

;; helper procedures
;; This section says it is okay to have another function be a wrapper to your recursive function.
(every-nth 3 '(with a little help from my friends))
;; defined like this:
(define (every-nth n sent)
  (every-nth-helper n n sent))

;; (every-nth-helper 3 3 '(with a little help from my friends))
(define (every-nth-helper interval remaining sent)
  (cond ((empty? sent) '())
        ((= remaining 1)
         (se (first sent)
             (every-nth-helper interval interval (bf sent))))
        (else (every-nth-helper interval (- remaining 1) (bf sent)))))

(define (every-nth-helper-r2 interval remaining sent outp)
  (display-all "calling every-nth-helper-r2 with interval: " interval ", remaining: " remaining ", sent: " sent ", outp: " outp)
  (cond ((empty? sent) outp)
        ((= remaining 1)
         (every-nth-helper-r2 interval interval (bf sent) (se (first sent) outp)))
        (else (every-nth-helper-r2 interval (- remaining 1) (bf sent) outp))))

;; How to use recursive patterns
(define (every-something sent)
  (if (empty? sent)
      '()
      (se (______ (first sent))
	  (every-something (bf sent)))))

;; this does not work
(define (every-something the-func sent)
  (if (empty? sent)
      '()
      (se (apply the-func (first sent))
	  (every-something (bf sent)))))
(every-something square '(2 3 4))

;; tail-recursive every
(define (every-tail-r sent outp)
  (display-all "calling every-tail-r with sent: " sent ", outp: " outp)
  (if (empty? sent)
      outp ;; base case returns output
      (every-tail-r (bf sent) (se outp (______ (first sent))))))

(define (keep-if-something sent)
  (cond ((empty? sent) '())
	((______? (first sent))
	 (se (first sent) (keep-if-something (bf sent))))
	(else (keep-if-something (bf sent)))))
;; tail-recursive keep
(define (keep-tail-r sent outp)
  (display-all "keep-tail-r with sent: " sent ", outp: " outp)
  (cond ((empty? sent) outp)
        ((______? (first sent))
         (keep-tail-r (bf sent) (se outp (first sent))))
        (else (keep-tail-r (bf sent) outp))))
(define (accumulate-somehow sent)
  (if (empty? sent)
      ______
      (______ (first sent)
              (accumulate-somehow (bf sent)))))
;; tail-recursive
(define (accumulate-r sent outp)
  (display-all "calling accumulate-r, sent: " sent ", outp: " outp)
  (if (empty? sent)
      outp
      (accumulate-r (bf sent) (word outp (first sent)))))

;; Exercises
;; Classify each of these problems as a pattern (every, keep, or accumulate), 
;; if possible, and then write the procedure recursively. 
;; In some cases we've given an example of invoking the procedure we want you to write, instead of describing it.
;; I will try to do these with tail-recursion.

;; 14.1  
;; > (remove-once 'morning '(good morning good morning))
;; (GOOD GOOD MORNING)
;; (It's okay if your solution removes the other MORNING instead, as long as it removes only one of them.) 
;; This is sort of like "keep." The result has one less, so it's not "every", and there is more than one, so it's not "accumulate".

(define (remove-once-r bad-word sent outp)
  (display-all "calling remove-once-r with bad-word: " bad-word ", sent: " sent ", outp: " outp)
  (cond ((empty? sent) outp)
        ((equal? bad-word (first sent)) (sentence outp (butfirst sent)))
        (else (remove-once-r bad-word (butfirst sent) (sentence  outp (first sent))))))

;;  14.2  
;; > (up 'town)
; (T TO TOW TOWN)
(define (up-r the-word outp)
  (display-all "calling up-r with the-word: " the-word ", and outp: " outp)
  (cond ( (= (count the-word) 0) outp)
        ( (= (count outp) 0) (up-r (butfirst the-word) (sentence (first the-word))))
        (else (up-r (butfirst the-word) (sentence outp (word (last outp) (first the-word)))))))
;; This is kind of like every. 

; 14.3  
; > (remdup '(ob la di ob la da))              ;; remove duplicates
; (OB LA DI DA)
; (It's okay if your procedure returns (DI OB LA DA) instead, as long as it removes all but one instance of each duplicated word.)
;; This is like "keep".
(define (remove-dup-r sent outp)
  (display-all "calling up-r with sent: " sent ", and outp: " outp)
  (cond ((empty? sent) outp)
        ((> (appearances (last sent) sent) 1) (remove-dup-r (butlast sent) outp))
        (else (remove-dup-r (butlast sent) (sentence (last sent) outp)))))

;;  14.4  
;; > (odds '(i lost my little girl))
;; (I MY GIRL)
;; This is like "keep" again
;; This needs a helper
(define (odds the-sent)
  (odds-r the-sent '() 1))

(define (odds-r sent outp counter)
  (display-all "odds are that you are calling odds-r with sent: " sent ", outp: " outp ", counter: " counter)
  (cond ( (empty? sent) outp)
        ((odd? counter) (odds-r (butfirst sent) (sentence outp (first sent)) (- counter 1)))
        (else (odds-r (butfirst sent) outp (- counter 1)))))

;;  14.5  [8.7] Write a procedure letter-count that takes a sentence as its argument and returns the total number of letters in the sentence:
;; > (letter-count '(fixing a hole))
;; 11
;; This one is accumulate.
;; When calling, set outp to 0
;; Or make your own helper.
(define (letter-count-r the-sent outp)
  (cond ((empty? the-sent) outp)
        (else (letter-count-r (butfirst the-sent) (+ outp (count (first the-sent)))))))

;; 14.6  Write member?.
;; This looks like accumulate
(define (member-r the-word the-sent)
  (member-r-helper the-word the-sent #f))
(define (member-r-helper the-word the-sent outp)
  (display-all "calling member-r-helper with the-word: " the-word ", the-sent: " the-sent ", outp: " outp)
  (cond ((empty? the-sent) outp)
        ((equal? the-word (first the-sent)) (member-r-helper the-word '() #t))
        (else (member-r-helper the-word (butfirst the-sent) #f))))

;; 14.7  Write differences, which takes a sentence of numbers as its argument 
and returns a sentence containing the differences between adjacent elements. 
;; (The length of the returned sentence is one less than that of the argument.)
;; So we do not go all the way through.
;; > (differences '(4 23 9 87 6 12))
;; (19 -14 78 -81 6)
;; Sort of like every
(define (differences-r the-nums outp)
  (display-all "calling differences-r with the-nums: " the-nums ", outp: " outp)
  (cond ((equal? (count the-nums) 1) outp)
        (else (differences-r (butfirst the-nums) (sentence outp (- (second the-nums) (first the-nums)))))))

;; 14.8  Write expand, which takes a sentence as its argument. 
;; It returns a sentence similar to the argument, 
;; except that if a number appears in the argument, 
;; then the return value contains that many copies of the following word:
;; > (expand '(4 calling birds 3 french hens))
;; (CALLING CALLING CALLING CALLING BIRDS FRENCH FRENCH FRENCH HENS)
;; > (expand '(the 7 samurai))
;; (THE SAMURAI SAMURAI SAMURAI SAMURAI SAMURAI SAMURAI SAMURAI)
;; I will have to make a helper function, that can also be recursive.
(define (print-n-times the-num the-word outp)
  ; (display-all "calling print-n-times with the-num: " the-num ", the-word: " the-word ", outp: " outp)
  (cond ((equal? 0 the-num) outp)
        (else (print-n-times (- the-num 1) the-word (sentence the-word outp)))))
(define (expand-r the-sent outp)
  (cond ((equal? (count the-sent) 0) outp)
        ((number? (first the-sent)) (expand-r (butfirst (butfirst the-sent)) (sentence outp (print-n-times (first the-sent) (second the-sent) '()))))
        (else (expand-r (butfirst the-sent) (sentence outp (first the-sent))))))




