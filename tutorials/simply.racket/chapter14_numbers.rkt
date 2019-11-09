#lang simply-scheme

; Chapter 14 Extra: Naming Numbers

(butfirst '(This is chapter 14 naming numbers))

;; break a string into sets of 3

(define (exponent-helper num group)
  (cond [(empty? num) '()]
        [(equal? group 1)  (sentence num)]
        [(equal? group 2)  (sentence num 'thousand)]
        [(equal? group 3)  (sentence num 'million)]
        [(equal? group 4)  (sentence num 'billion)]
        [(equal? group 5)  (sentence num 'trillion)]
        [(equal? group 6)  (sentence num 'quadrillion)] 
        [(equal? group 7)  (sentence num 'quintillion)]
        [(equal? group 8)  (sentence num 'sextillion)] 
        [(equal? group 9)  (sentence num 'septillion)] 
        [(equal? group 10) (sentence num 'octillion)]
        [(equal? group 11) (sentence num 'nonillion)]
        [(equal? group 12) (sentence num 'decillion)]))

; "item" in Simply Scheme starts counting a list at "1", not "0" like C
(define number-table '(one two three four five six seven eight nine zero))

(define power-1-table '(ten twenty thirty forty fifty sixty seventy eighty ninety))

(define (get-last-3 the-num)
  (word (last (butlast (butlast the-num)))
        (last (butlast the-num))
        (last the-num)))

(define (remove-leading-zeros num outp)
  (cond [(empty? num) outp]
        [(zero? (first num)) (remove-leading-zeros (butfirst num) outp)]
        [else num]))

(define (do-string-break num-string num-list)
  (cond [(<= (count num-string) 3)  (sentence num-string num-list)]
        [else (do-string-break (butlast (butlast (butlast num-string)))
                               (sentence (remove-leading-zeros (word (get-last-3 num-string)) "")
                                         num-list))]))

(define (get-new-teen-table n)
  (cond [(empty? n) 'ten]
        [(equal? (first n) 'one) 'eleven]
        [(equal? (first n) 'two) 'twelve]
        [(equal? (first n) 'three) 'thirteen]
        [else (word (first n) 'teen)]))

(define (num-name2-work num-work output depth)
  (cond [(empty? num-work) output]        ;; skip ending zero
        [(zero? (last num-work)) (num-name2-work (butlast num-work) 
                                                 output 
                                                 (+ 1 depth))]
        [(equal? depth 1) (num-name2-work (butlast num-work) 
                                          (sentence (item (last num-work) number-table)) 
                                          (+ 1 depth))]
        ; new teens
        [(and (equal? depth 2) (equal? 1 (last num-work))) (num-name2-work (butlast num-work) 
                                                                           (get-new-teen-table output) 
                                                                           (+ 1 depth))]
        [(equal? depth 2) (num-name2-work (butlast num-work) 
                                          (sentence (item (last num-work) power-1-table)  output) 
                                          (+ 1 depth))]
        [(equal? depth 3) (num-name2-work (butlast num-work) 
                                          (sentence (item (last num-work) number-table) 'hundred output) 
                                          (+ 1 depth))]))

(define (grand-num-name-worker the-num outp group)
  (cond [(empty? the-num) outp]
        [else (grand-num-name-worker (butlast the-num)
                                     (sentence (exponent-helper (num-name2-work (last the-num) "" 1) 
                                                                group) outp) 
                                     (+ 1 group))]))

(define (grand-num-name-caller raw-num)
  (keep (lambda (x) (not (empty? x)))
        (grand-num-name-worker (do-string-break (number->string raw-num) '()) "" 1)))

(module+ test
  (require rackunit)
  (check-true #t)

  (printf "(do-string-break \"12345\" '()): ~a \n" (do-string-break "12345" '()))
  (check-equal? (do-string-break "12345" '()) 
                '(12 345)
                "Error for: (do-string-break \"12345\" '())")

  (printf "(get-last-3 '4567): ~a \n" (get-last-3 '4567))
  (check-equal? (get-last-3 '4567) 
                567 
                "Error for: (get-last-3 '4567)")
  (printf "(exponent-helper '(five hundred thirteen) 2): ~a \n" (exponent-helper '(five hundred thirteen) 2))
  (check-equal? (exponent-helper '(five hundred thirteen) 2) '(five hundred thirteen thousand) "Error for: (exponent-helper '(five hundred thirteen) 2)")



  (printf "(grand-num-name-caller 1428425): ~a \n" (grand-num-name-caller 1428425))
  (check-equal? (grand-num-name-caller 1428425) 
                '(one million four hundred twenty eight thousand four hundred twenty five)  
                "Error for: (grand-num-name-caller 1428425)")

  (printf "(grand-num-name-caller 1000529): ~a \n" (grand-num-name-caller 1000529))
  (check-equal? (grand-num-name-caller 1000529) 
                '(one million five hundred twenty nine)  
                "Error for: (grand-num-name-caller 1000529)")

  (printf "(grand-num-name-caller 5513345): ~a \n" (grand-num-name-caller 5513345))
  (check-equal? (grand-num-name-caller 5513345) 
                '(five million five hundred thirteen thousand three hundred forty five) 
                "Error for: (grand-num-name-caller 5513345)")
) ;; end module+ test 

