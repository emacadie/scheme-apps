;; break a string into sets of 3

(define (exponent-helper group)
  (display-all "calling exponent-helper with group: " group)
  (cond ((equal? the-ex 2) 'thousand)
        ((equal? the-ex 3) '(million))
        ((equal? the-ex 4) '(billion))
        ((equal? the-ex 5) '(trillion))))
; "item" in Simply Scheme starts counting a list at "1", not "0" like C
(define number-table '(one two three four five six seven eight nine zero))

(define teen-table '(eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen))

(define (get-teen-table the-num)
  (if (equal? the-num 0) 
      'ten
      (item the-num teen-table)))

(define power-1-table '(ten twenty thirty forty fifty sixty seventy eighty ninety))

(define (power-1-helper the-num)
  (item the-num power-1-table))

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
                               (sentence (remove-leading-zeros 
                                          (word (get-last-3 num-string)) 
                                          "" )
                                         num-list)))))

(define (get-new-teen-table n)
  (display-all "get-new-teen-table: " n)
  (cond ((empty? n) 'ten) 
        ((equal? (first n) 'one) 'eleven)
        ((equal? (first n) 'two) 'twelve)
        ((equal? (first n) 'three) 'thirteen)
        (else (word (first n) 'teen))))

(define (num-name2-work num-work output depth)
  (display-all "num-name2-work with num-work: " num-work ", output: " output ", depth: " depth)
  (cond ((empty? num-work) output)
        ;; skip ending zero
        ((zero? (last num-work)) (num-name2-work (butlast num-work) output (+ 1 depth)))
        ((equal? depth 1) (num-name2-work (butlast num-work) (sentence (item (last num-work) number-table)) (+ 1 depth)))
        ; new teens
        ((and (equal? depth 2) (equal? 1 (last num-work))) (num-name2-work (butlast num-work) (get-new-teen-table output) (+ 1 depth)))
        ((equal? depth 2) (num-name2-work (butlast num-work) (sentence (item (last num-work) power-1-table)  output) (+ 1 depth)))
        ((equal? depth 3) (num-name2-work (butlast num-work) (sentence (item (last num-work) number-table) 'hundred output) (+ 1 depth)))))

(define (number-name-2 number)
  (num-name2-work (last (do-string-break (number->string number) '())) 
                  "" 
                  1)
)
; break into threes

