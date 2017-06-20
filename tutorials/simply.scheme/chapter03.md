From the book: One of the biggest problems that beginning Lisp programmers have comes from trying to read a program from left to right, rather than thinking about it in terms of expressions and subexpressions.    
So true, my friend.   

3.1: Translate the arithmetic expressions (3+4)×5 and 3+(4×5) into Scheme expressions, and into plumbing diagrams.   
The first one should come out to 7 x 5, so it would be 
```scheme
(* 5 (+ 3 4))
```
The second one is 20 + 3, so it would be
```scheme
(+ 3 (* 4 5))
```

3.2: How many little people does Alonzo hire in evaluating each of the following expressions: 
First off, I think some of the metaphors are a bit stupid. But I think each function call is a little person.   
```scheme
(+ 3 (* 4 5) (- 10 4))
```
has three operations, so three LP.   
 
```scheme
(+ (* (- (/ 8 2) 1) 5) 2)
```
has four operations.   
```scheme 
(* (+ (- 3 (/ 4 2))
      (sin (* 3 2))
      (- 8 (sqrt 5)))
   (- (/ 2 3)
      4))
```
has 10 function calls/operations.   

3.3: Each of the expressions in the previous exercise is compound. How many subexpressions (not including subexpressions of subexpressions) does each one have?   
For example,   
```scheme
(* (- 1 (+ 3 4)) 8)
```
has three subexpressions; you wouldn't count (+ 3 4).   
So I guess (+ 3 4) is a sub-subexpression, and a subexpression is just one level down?
1. *
2. -
3. 8
So then subexpressions are the function and its args? Okay by me.   
```scheme
(+ 3 (* 4 5) (- 10 4))
```
has four subexpressions.

```scheme
(+ (* (- (/ 8 2) 1) 5) 2)
```
has three operations.
```scheme
(* (+ (- 3 (/ 4 2))
      (sin (* 3 2))
      (- 8 (sqrt 5)))
   (- (/ 2 3)
      4))
```
has two subexpressions using my definition.  

 3.4: Five little people are hired in evaluating the following expression:   
```scheme
(+ (* 3 (- 4 7))
   (- 8 (- 3 5)))
```
Give each little person a name and list her specialty, the argument values she receives, her return value, and the name of the little person to whom she tells her result.   

I really think this whole "little people" thing is kind of dumb. (And kind of rude if you're really short.)  
Andrea Addition will get the first task, and she sees multiplication and subtraction. So we call Tina Times and Mary Minus.    
Tina Times sees that she will need the result of another subtraction, so we call Susan Subtraction. Susan subtracts 7 from 4, and gives us -3. So we give Tina -3. She uses that in her multiplication function along with the 3, and we get -9.   
Mary Minus sees that she also has a subtraction, so we call Sarah Subtraction. (She is not related to Susan. Like South Korea, about (/ 1 4) of all people in Simply Scheme Land are named "Subtraction".) Sarah looks at her arguments, and gives us -2. Mary takes that, puts it in her function with the 8, and gets 10.  
Then we take -9 and 10 and give them to Andrea, and she comes up with 1.   

3.5: Evaluate each of the following expressions using the result replacement technique:    
```scheme
(sqrt (+ 6 (* 5 2)))
```
Here we go:   
```
(sqrt (+ 6 (* 5 2)))
(sqrt (+ 6 10))
(sqrt 16)
4.0
```
 Next:   
```scheme
(+ (+ (+ 1 2) 3) 4)
```
Here we go:   
```
(+ (+ (+ 1 2) 3) 4)
(+ (+ 3 3) 4)
(+ 6 4)
10
```
3.6: I made some diagrams.   

3.7: What value is returned by (/ 1 3) in your version of Scheme? (Some Schemes return a decimal fraction like 0.33333, while others have exact fractional values like 1/3 built in.)   
Result:   
```
#;18> (/ 1 3)
1/3
```

3.8: Which of the functions that you explored in Chapter 2 will accept variable numbers of arguments?    
It looks to me like none of them take variable numbers of arguments. I assume they are talking about the functions in the "functions" program. If they are talking about regular Scheme, then a lot of the functions take variable arguments, like many math functions (+, -, you name it).   

3.9: The expression (+ 8 2) has the value 10. It is a compound expression made up of three atoms. For this problem, write five other Scheme expressions whose values are also the number ten:  
- An atom 10
- Another compound expression made up of three atoms (- 15 5)
- A compound expression made up of four atoms (+ 5 3 2)
- A compound expression made up of an atom and two compound subexpressions (+ (+ 4 3) (- 15 12))
- Any other kind of expression  (count '(0 1 2 3 4 5 6 7 8 9))


