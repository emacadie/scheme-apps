This chapter is about procedures.   

Procedures in Scheme have four parts: the word "define", the name, the parameters, the body.   

A "function" is what a procedure does, "process" is how it does it. Take these two:  
```
(define (f x)
  (+ (* 3 x) 12))

(define (g x)
  (* 3 (+ x 4)))
```
They give the same result, so they perform/represent the same function, but they do it differently, so they are different procedures. I think this is a slightly different definition of "function", but then I am not a mathematician.   

Parameter vs. argument: A procedure definition has parameters; a specific call has arguments.   

Note: Scheme does not have an "average" function. Probably because you can average more than two values.  

```
(define (average a b)
  (/ (+ a b) 2))
```   

Composability: the result of a procedure can be the argument to the next procedure.    

Two pitfalls: procedures can only return one value, and be careful with name collision. I know the Scheme people want their language to be simple, but perhaps it is time for namespaces.   

4.1: Consider this procedure:
```
(define (ho-hum x y)
  (+ x (* 2 y)))
```
Show the substitution that occurs when you evaluate
```
(ho-hum 8 12)
```
First, you get 
```
(+ 8 (* 2 12))
```
Then you get
```
(+ 8 24)
```
Then you get
```
32
```

Trying it out in Chicken REPL:   
```
#;1> (define (ho-hum x y)
  (+ x (* 2 y)))
#;2> (ho-hum 8 12)
32
#;3> 
```

4.2: Given the following procedure:   
```
(define (yawn x)
  (+ 3 (* x 2)))
```
list all the little people that are involved in evaluating
```
(yawn (/ 8 2))
```
Give their names, their specialties, their arguments, who hires them, and what they do with their answers.   

More of the "little people" bit.  

We need Annie Addition, Mary Multiply, and Daisy Division. So, substituting for x, we have this:
```
(+ 3 (* (/ 8 2) 2))
```
Annie calls Mary, who calls Daisy for the division. Daisy does her work, gets 4, and gives Mary this:  
```
(+ 3 (* 4 2))
```
Then Mary does her work, gets 8, and gives Annie this:
```
(+ 3 8)
```
Then Annie finishes the work and gives us 11.   

In the Chicken REPL:  
```
#;3> (define (yawn x)
  (+ 3 (* x 2)))
#;4> (yawn (/ 8 2))
11
```

4.3: Here are some procedure definitions. For each one, describe the function in English, show a sample invocation, and show the result of that invocation.   
```
(define (f x y) (- y x))
```
This is the built-in function "-" with the parameters reversed. I am honestly not clear why you would ever do that.  
```
#;5> (define (f x y) (- y x))
#;6> (f 5 3)
-2
#;7> (- 3 5)
-2
```
Next:
```
(define (identity x) x)
```
This just returns its argument. There is a function in Clojure called "identity" that does the same thing.  
```
#;8> (define (identity x) x)
#;9> (identity "hello this function is just like the Clojure function")
"hello this function is just like the Clojure function"
#;10> (identity 44)
44
```
Next:   
```
(define (three x) 3)
```
This just returns the number 3. Like all those static strings in all those Java classes. Why would "three" mean anything other than 3?   
```
#;11> (define (three x) 3)
#;12> (three "life is meaningless when you are a string that is ignored in a program")
3
#;13> (three 4)
3
```
Next:   
```
(define (seven) 7)
```
Pretty much the same as above, except without a parameter that is ignored. "Scheme's seven procedure eliminates the middle man and passes the savings on to you!"   
```
#;14> (define (seven) 7)
#;15> (seven)
7
```
Next:   
```
(define (magic n)
  (- (/ (+ (+ (* 3 n)
              13)
           (- n 1))
        4)
     3))
```
Let's reformat this one:   
```
(define (magic n)
  (- (/ (+ (+ (* 3 n) 13) (- n 1)) 4) 3))
```
Both places where the parameter is used are pretty central. I don't know which one gets called first. I will have to go through this step-by-step.   
Let's go through this with the argument 5 for "n".   
First, we multiply 5 by 3, and we get this:
```
(- (/ (+ (+ 15 13) (- n 1)) 4) 3)
```
Then we subtract 1 from 5, and we get this:  
```
(- (/ (+ (+ 15 13) 4) 4) 3)
```
Then we add 15 and 13, and we get this:  
```
(- (/ (+ 28 4) 4) 3)
```
Then we add 28 and 4:
```
(- (/ 32 4) 3)
```
Then we divide 32 by 4:  
```
(- 8 3)
```
Then we subtract 3 from 8:
```
5
```
In the Chicken REPL:
```
#;16> (define (magic n)
  (- (/ (+ (+ (* 3 n)
              13)
           (- n 1))
        4)
     3))
#;17> (magic 5)
5
```
So it just returns the argument. As Pee-Wee Herman might say: I knew that.  

4.4: Each of the following procedure definitions has an error of some kind. Say what's wrong and why, and fix it:   
```
(define (sphere-volume r)
  (* (/ 4 3) 3.141592654)
  (* r r r))
```
This will only return the function on the second line: (* r r r). Try this:
```
(define (sphere-volume r)
  (* (/ 4 3) 3.141592654 (* r r r)))
```
According to http://www.calculateme.com/cVolume/VolumeOfSphere.htm, a sphere with a radius of 10 has a volume of 4,188.790205.
```
#;19> (define (sphere-volume r)
 (* (/ 4 3) 3.141592654 (* r r r)))  
#;20> (sphere-volume 10)
4188.79020533333
```
For 23.5, they get 54,361.595679, I get 54361.5956860403. Perhaps they are using a different number of digits for pi.  
```
(define (next x)
  (x + 1))
```
This is supposed to add one to a number. For Scheme, (x + 1) should be (+ x 1).
```
#;23> (define (next x)
   (+ x 1))  
#;24> (next 5)
6
```
Next:
```
(define (square)
  (* x x))
```
This one does not have a parameter in the definition. If you type it in as-is, Chicken REPL tells you that both instances of "x" are unbound. We can fix this by using the "square" from the beginning of this chapter:
```
(define (square x)
  (* x x))
```
Next:

This does not have any way to go from the "triangle" parameter to "base" and "height". Here is a better way:   
```
#;26> (define (triangle-area base height)
  (* 0.5 base height))
#;27> (triangle-area 22 10)
110.0
#;28> 
```
Next:   
```
(define (sum-of-squares (square x) (square y))
  (+ (square x) (square y)))
```
You cannot call a function in the parameters of a function, only in the body.  
To remedy, you will need the square function from above in your session, then change the parameters to just x and y:

```
#;2> (define (sum-of-squares x y)
  (+ (square x) (square y)))
#;3> (sum-of-squares 3 4)
25
```

4.5: Write a procedure to convert a temperature from Fahrenheit to Celsius, and another to convert in the other direction. The two formulas are F=9⁄5C+32 and C=5⁄9(F-32).   
```
(define (c-to-f c)
    (inexact (+ 32 (* (/ 9 5) c))))
(define (f-to-c f)
    (* (/ 5 9) (- f 32)))
```


4.6: Define a procedure fourth that computes the fourth power of its argument. Do this two ways, first using the multiplication function, and then using square and not (directly) using multiplication. 
```
(define (fourth x)
    (* x (* x (* x x))))
```

