Exercises for Chapter two of "Simply Scheme": https://people.eecs.berkeley.edu/~bh/ss-toc2.html  

Functions in this chapter:   

|function    |arg 1 type|arg 2 type|arg3 type|result type|
|---         | ---      |---       |---       |---       |
|+           |number    |number    |          |number    |
|-           |number    |number    |          |number    |
|/           |number    |number    |          |number    |
|\<=         |number    |number    |          |boolean   |
|\<          |number    |number    |          |boolean   |
|=           |number    |number    |          |boolean   |
|\>=         |number    |number    |          |boolean   |
|\>          |number    |number    |          |boolean   |
|and         |boolean   |boolean   |          |boolean   |
|appearances |word      |sentence  |          |number    |
|cos         |number    |          |          |number    |
|count       |*anything*|          |          |number    |
|even?       |*anything*|          |          |boolean   |
|equal?      |*anything*|*anything*|          |boolean   |
|every       |*name of function*|*that function's inputs*|*anything*|
|expt        |number    |number    |          |number    |
|if          |boolean   |*anything*|*anything*|*anything*|
|member?     |word      |sentence  |          |boolean   |
|not         |boolean   |          |          |boolean   |
|number-of-arguments|*name of function*||     |number    |
|odd?        |*anything*|          |          |boolean   |
|quotient    |number    |number    |          |number    |
|random      |number    |          |          |number    |
|remainder   |number    |number    |          |number    |
|sqrt        |number    |          |          |number    |
|vowel?      |*anything*|          |          |boolean   |
|word        |*word or integer*|*word or integer*||word  |


- butfirst
- butlast
- first
- item
- keep
- last
- max
- not
- number?
- or
- round
- sentence

For "and", "not" and "or", I have to enter the booleans as "#t" or "#f". That did not occur to me right away. Usually you get the values for the arguments from a function, or multiple functions. I guess this "functions" program does things differently.    
    
Chapter two:   
2.1:  

|function|arg 1|arg 2|result|
|--- | --- |--- |--- |
|word|now|here|*nowhere*|
|sentence|now|here|*(now here)*|
|first|blackbird|~~none~~|*b*|
|first|(blackbird)|~~none~~|*blackbird*|
|*+*|3|4|7|
|every|*first*|(thank you girl)|(hank ou irl)| 
|member?|e|aardvark|*#F*|
|member?|the|*(the best language is scheme)*|#t
|keep|vowel?|(i will)|*(i)*|
|keep|vowel?|*perihelion*|eieio|
|last|()|~~none~~|~~*Argument(s) not in domain.*~~|	
|*every*|last|(honey pie)|(y e)|
|*keep*|*vowel?*|taxman|aa|

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


