;; 4: Defining functions

;; if run in top dir of git repo
(load-relative "tutorials/shido_yat/hello.scm")

;; hello with name
(define hello
    (lambda (name)
        (string-append "Hello " name "!")))

(define sum3
    (lambda (a b c)
        (+ a b c)))

;; you can also define functions without lambda
(define (hello name)
    (string-append "Hello " name "!"))

(define (sum3 a b c)
    (+ a b c))

;; exercise 1

;; A function that add 1 to the argument.
(define (add1 a)
    (+ a 1))
;; A function that subtract 1 from the argument. 
(define (subtract1 a)
    (- a 1))
(add1 22) ;; 23
(subtract1 22) ;; 21

;; exercise 2
;; Let's write a function to calculate flying distances with following steps.
;; 1 - Write a function that convert the unit of angles from degree to radian.
;; 180 degree is π radian. π is defined by
;; (define pi (* 4 (atan 1.0)))
;; 
(define pi (* 4 (atan 1.0)))
(define (deg-to-rad deg)
    (* deg (/ pi 180.0)))
;; 2 - Write a function that calculates a distance of moving at a constant velocity (vx) in t seconds.
(define (distance-from-vel-and-time vx t)
    (* vx t))

;; 3 - Write a function that calculates a duration till the object reach the ground that is launched with vertical velocity of vy.
;; Ignore air drag and use 9.8 m s-2 (squared) as the value of acceleration of gravity (g).
;; hint: As the vertical velocity when the object reaches the ground is -vy,
;; 2 vy = g t
;; where t is the falling duration.
(define (free-fall-time vy)
    (/ (* 2 vy) 9.8))
    
;; 4 - Write a function that calculates a flying distance of a ball thrown with an initial velocity v and an angle theta degree using functions defined in questions 1--3.
;; Hint: First, convert degree to radian (say theta1). The horizontal and vertical initial velocities are represented by
;; v cos(theta1) and v sin(theta1), respectively. The duration of falling can be calculated by the function of the question 3. As horizontal velocity doesn't change, the distance can be calculated using the function of the question 2.

(define (calc-ball-distance v-init theta)
    (let ((v-hor (cos (deg-to-rad theta)))
            (v-ver (sin (deg-to-rad theta)))
            (fall-dur (free-fall-time v-ver)))
        (distance-from-vel-and-time v-hor fall-dur)))

(define (calc-ball-distance v-init theta)
    (let* ((theta1 (deg-to-rad theta))
            (v-hor (cos theta1))
            (v-ver (sin theta1))
            (fall-dur (free-fall-time v-ver)))
        (distance-from-vel-and-time v-hor fall-dur)))
;; 5 - Calculate the distance of a ball thrown with a initial velocity of 40 m s-1 and an angle of 30 degree. It corresponds to the distance of a long cast by a professional baseball player having a strong arm. 
;; his/her (?) answer
(define (distance v ang)
  (distance-from-vel-and-time
   (* v (cos (deg-to-rad ang)))                     ; vx
   (free-fall-time (* v (sin (deg-to-rad ang))))))         ; t
;; close, right? multiply both inputs by v since it's 40 meters/second SQUARED
(distance 40 30)
;Value: 141.39190265868385

