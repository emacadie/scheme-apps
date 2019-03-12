;; break a string into sets of 3
(define (break-into-threes the-num)
  (do-string-break (number->string the-num) '()))

(define (remove-leading-zeros num outp)
  (cond ((empty? num) outp) 
        ((zero? (first num)) (remove-leading-zeros (butfirst num) outp))
        (else num)))

(define (do-string-break num-string num-list)
  (display-all "do-string-break, num-string: " num-string ", num-list: " num-list)
  (cond ((<= (count num-string) 3)  (sentence num-string num-list))
        (else (do-string-break (butlast (butlast (butlast num-string)))
                               (sentence (remove-leading-zeros 
                                          (word (last (butlast (butlast num-string))) (last (butlast num-string)) (last num-string)) 
                                          "" )
                                         
                                         num-list)))))

