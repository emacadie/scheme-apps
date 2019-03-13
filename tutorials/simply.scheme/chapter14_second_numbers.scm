;; break a string into sets of 3

(define (exponent-helper num group)
  (cond ((empty? num) '())
        ((equal? group 1) (sentence num))
        ((equal? group 2) (sentence num 'thousand))
        ((equal? group 3) (sentence num 'million))
        ((equal? group 4) (sentence num 'billion))
        ((equal? group 5) (sentence num 'trillion))))
; "item" in Simply Scheme starts counting a list at "1", not "0" like C
(define number-table '(one two three four five six seven eight nine zero))

(define power-1-table '(ten twenty thirty forty fifty sixty seventy eighty ninety))

(define (get-last-3 the-num)
  (word (last (butlast (butlast the-num)))
        (last (butlast the-num))
        (last the-num)))

(define (remove-leading-zeros num outp)
  (cond ((empty? num) outp) 
        ((zero? (first num)) (remove-leading-zeros (butfirst num) outp))
        (else num)))

(define (do-string-break num-string num-list)
  (cond ((<= (count num-string) 3)  (sentence num-string num-list))
        (else (do-string-break (butlast (butlast (butlast num-string)))
                               (sentence (remove-leading-zeros (word (get-last-3 num-string)) "")
                                         num-list)))))

(define (get-new-teen-table n)
  (cond ((empty? n) 'ten) 
        ((equal? (first n) 'one) 'eleven)
        ((equal? (first n) 'two) 'twelve)
        ((equal? (first n) 'three) 'thirteen)
        (else (word (first n) 'teen))))

(define (num-name2-work num-work output depth)
  (cond ((empty? num-work) output)
        ;; skip ending zero
        ((zero? (last num-work)) (num-name2-work (butlast num-work) output (+ 1 depth)))
        ((equal? depth 1) (num-name2-work (butlast num-work) (sentence (item (last num-work) number-table)) (+ 1 depth)))
        ; new teens
        ((and (equal? depth 2) (equal? 1 (last num-work))) (num-name2-work (butlast num-work) (get-new-teen-table output) (+ 1 depth)))
        ((equal? depth 2) (num-name2-work (butlast num-work) (sentence (item (last num-work) power-1-table)  output) (+ 1 depth)))
        ((equal? depth 3) (num-name2-work (butlast num-work) (sentence (item (last num-work) number-table) 'hundred output) (+ 1 depth)))))

(define (number-name-2 number)
  (num-name2-work (last (do-string-break (number->string number) '())) "" 1))

(define (grand-num-name-worker the-num outp group)
  (cond ((empty? the-num) outp)
        (else (grand-num-name-worker (butlast the-num)
                                     (sentence (exponent-helper (num-name2-work (last the-num) "" 1) group) outp) 
                                     (+ 1 group)))))

(define (grand-num-name-caller raw-num)
  (keep (lambda (x) (not (empty? x)))
        (grand-num-name-worker (do-string-break (number->string raw-num) '()) "" 1)))


