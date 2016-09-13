;; 06 local variables

;; Example 1: Declaring local variables i and j, binding them to 1 and 2 and then making a sum of them. 
(let ((i 1) (j 2))
    (+ i j))
;; 3

;; Example 2: Making local variables i and j, binding them to 1 and i+2, and multiplying them. 
(let ((i 1))
    (let ((j (+ i 2)))
        (* i j)))
;; also 3

;; let* does the same thing as nested let expressions - syntactic sugar for nested let expressions
(let* ((i 1)
        (j (+ i 2)))
    (* i j))
;; also 3

;; Example 3: A function quadric-equation that calculates the answers of quadratic equations.
;; It takes three coefficient a, b, c (a x2 + b x + c = 0) as arguments and returns the list of answers in real numbers. 
;; The answers can be calculated without useless calculations by using let expressions step by step.
(define (quadratic-equation a b c)
    (if (zero? a)
        'error
        (let ((d (- (* b b) (* 4 a c))))
            (if (negative? d)
                '()
                (let ((e (/ b a -2)))
                    (if (zero? d)
                        (list e)
                        (let ((f (/ (sqrt d) a 2)))
                            (list (+ e f) (- e f)))))))))
(quadratic-equation 3 5 2) ;; (-0.6666666666666667 -1.0) in kawa, (-2/3 -1) in guile and chez

;; Write a function described at exercise 2 in chapter 4 with one function,
;; which means that writing a function to calculate flying distances with initial velocity of v and angle to the surface of a.

; definition of pi
(define pi (* 4 (atan 1.0)))

; degree -> radian
(define (radian deg)
  (* deg (/ pi 180.0)))

; free fall time
(define (ff-time vy)
  (/ (* 2.0 vy) 9.8))

; horizontal distance 
(define (dx vx t)
  (* vx t))

; distance
(define (distance v ang)
    (let* ((pi (* 4 (atan 1.0)))
            (radian-ang (* ang (/ pi 180.0)))
            )
  (dx
   (* v (cos radian-ang))                     ; vx
   (ff-time (* v (sin radian-ang))))))         ; t

(distance 40 30) ;; 141.39190265868385

