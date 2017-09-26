Exercises for Chapter two of "Simply Scheme": https://people.eecs.berkeley.edu/~bh/ss-toc2.html  

Functions in this chapter:   

|function    |arg 1 type|arg 2 type|arg3 type|result type|
|---         |---       |---       |---       |---       |
|+           |number    |number    |          |number    |
|-           |number    |number    |          |number    |
|/           |number    |number    |          |number    |
|*           |number    |number    |          |number    |
|\<=         |number    |number    |          |boolean   |
|\<          |number    |number    |          |boolean   |
|=           |number    |number    |          |boolean   |
|\>=         |number    |number    |          |boolean   |
|\>          |number    |number    |          |boolean   |
|and         |boolean   |boolean   |          |boolean   |
|appearances |word      |sentence  |          |number    |
|butfirst    |number    |          |          |number    |
|butfirst    |sentence  |          |          |word      |
|butfirst    |word      |          |          |*all but first letter in word*|
|butlast     |number    |          |          |number    |
|butlast     |sentence  |          |          |word      |
|butlast     |word      |          |          |*all but last letter in word*|
|cos         |number    |          |          |number    |
|count       |*anything*|          |          |number    |
|even?       |*anything*|          |          |boolean   |
|equal?      |*anything*|*anything*|          |boolean   |
|every       |*name of function*|*that function's inputs*|*anything*|
|expt        |number    |number    |          |number    |
|first       |number    |          |          |number    |
|first       |sentence  |          |          |word      |
|first       |word      |          |          |*first letter in word*|
|if          |boolean   |*anything*|*anything*|*anything*|
|item        |number    |sentence  |          |word      |
|keep        |*name of function returning boolean*|word| |word      |
|last        |number    |          |          |number    |
|last        |sentence  |          |          |word      |
|last        |word      |          |          |*last letter in word*|
|max         |number    |number    |          |number    |
|min         |number    |number    |          |number    |
|member?     |word      |sentence  |          |boolean   |
|member?     |letter    |word      |          |boolean   |
|not         |boolean   |          |          |boolean   |
|number?     |*anything*|          |          |boolean   |
|number-of-arguments|*name of function*||     |number    |
|odd?        |*anything*|          |          |boolean   |
|or          |boolean   |boolean   |          |boolean   |
|quotient    |number    |number    |          |number    |
|random      |number    |          |          |number    |
|remainder   |number    |number    |          |number    |
|round       |number    |          |          |number    |
|sentence    |*anything*|*anything*|          |sentence  |
|sqrt        |number    |          |          |number    |
|vowel?      |*anything*|          |          |boolean   |
|word        |*word or integer*|*word or integer*||word  |



For "and", "not" and "or", I have to enter the booleans as "#t" or "#f". That did not occur to me right away. Usually you get the values for the arguments from a function, or multiple functions. I guess this "functions" program does things differently.    

Some of the functions in the chart above are part of R7RS, and some are unique to Simply Scheme.    
Also: There are some aliases to some Simply Scheme functions:   
|function|alias|
|---     |---  |
|butfirst|bf   |
|butlast |bl   |
|sentence|se   |
    
Chapter two:   
2.1:  

|function|arg 1      |arg 2           |result       |
|---     | ---       |---             |---          |
|word    |now        |here            |*nowhere*    |
|sentence|now        |here            |*(now here)* |
|first   |blackbird  |~~none~~        |*b*          |
|first   |(blackbird)|~~none~~        |*blackbird*  |
|*+*     |3          |4               |7            |
|every   |*first*    |(thank you girl)|(hank ou irl)| 
|member? |e          |aardvark        |*#F*         |
|member? |the        |*(the best language is scheme)*|#t|
|keep    |vowel?     |(i will)        |*(i)*        |
|keep    |vowel?     |*perihelion*    |eieio        |
|last    |()         |~~none~~        |~~*Argument(s) not in domain.*~~|	
|*every* |last       |(honey pie)     |(y e)        |
|*keep*  |*vowel?*   |taxman          |aa           |

2.2:   
He talks about the domain (possible arguments) and range (return values) of functions. I have never heard those terms before. The domain of the vowel? function is a string (which he calls a word) or a list without the tick (which I think he calls a sentence). He also defines functions as a data type.     
2.3:   
The domain of "appearances" seems to be a word (or a number), then a sentence, and the range is the number of times the word is in the sentence.

2.4:  
The domain of "item" is a number, then a sentence, and the range is the item in the sentence that is at the place in the sequence corresponding to the given number:    
```
Function: item
Argument: 3
Argument: (Scheme is the best language)

The result is: the

Function: item
Argument: 4
Argument: (I want to learn Scheme)

The result is: learn

Function: item
Argument: 7
Argument: (Scheme is the best)
Argument(s) not in domain.
```
2.5: List the one-argument functions in this chapter for which the type of the return value is always different from the type of the argument.  
As far as I can tell, only number-of-arguments fits that criteria.   

2.6: List the one-argument functions in this chapter for which the type of the return value is sometimes different from the type of the argument.   
butfirst, butlast, count, even?, first, last, odd, vowel?    

2.7: Mathematicians sometimes use the term "operator" to mean a function of two arguments, both of the same type, that returns a result of the same type. Which of the functions you've seen in this chapter satisfy that definition?   
+, -, /, *, and, expt, max, min, or, quotient, remainder. Mostly math functions.    

2.8: An operator f is commutative if f(a,b)=f(b,a) for all possible arguments A and B. For example, + is commutative, but word isn't. Which of the operators from Exercise 2.7 are commutative?    
+, *, and, max, min   

2.9: An operator f is associative if f(f(a,b),c)=f(a,f(f(b,c)) for all possible arguments A, B, and C. For example, * is associative, but not /. Which of the operators from Exercise 2.7 are associative?    
Again, I think the answer is: +, *, and, max, min   


