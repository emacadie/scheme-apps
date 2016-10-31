;; 14 - vectors and structures

;; vectors represeneted by #() which is for anonymous functions in Clojure
;; #(1 2 3)

;; shido's function to calculate sum of vectors
(define (vector-add v1 v2)
    (let ((lenv1 (vector-length v1))
            (lenv2 (vector-length v2)))
        (if (= lenv1 lenv2)
            (let ((v (make-vector lenv1)))
                (let loop ((i 0))
                    (if (= i lenv1)
                        v
                        (begin
                            (vector-set! v i (+ (vector-ref v1 i) (vector-ref v2 i)))
                            (loop (+ 1 i))))))
            (error "different dimensions"))))

(vector-add #(1 2 3) #(4 5 6)) ;; 
#(5 7 9)

;; Exercise 1
;; Write a function that calculates the inner product of two vectors. 
;; like map, or reduce?
;; turns out it's both
(define (inner-product vec1 vec2)
       (let ((len1 (vector-length vec1))
     	(len2 (vector-length vec2)))
         (if (= len1 len2)
     	(let loop ((i 0) (pro 0))
     	  (if (= i len1)
     	      pro
     	      (loop (+ 1 i) (+ pro
     			      (* (vector-ref vec1 i)
     				 (vector-ref vec2 i))))))
     	(error "different dimensions."))))

(inner-product  #(1 2 3) #(4 5 6)) ;; 32
(require 'list-lib)
(reduce + 0 (map * #(1 2 3) #(4 5 6)))

;; 3 structures
;;; simple structure definition, from next section does not work in Kawa or guile

;; it seems we are hitting the limits of usefulness of this tutorial
;; it's a good tutorial, but a bit old (some of the links do not work)
;; and some parts are implementation-specific
;; for all its warts, Common Lisp is looking pretty good


