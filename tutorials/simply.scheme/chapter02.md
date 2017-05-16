Exercises for Chapter two of "Simply Scheme": https://people.eecs.berkeley.edu/~bh/ss-toc2.html  

Functions in this chapter:   
+, -, /, <=, <, =, >=, >, and, appearances, butfirst, butlast, cos, count, equal?, every, even?, expt, first, if, item, keep, last, max, member?, not, number?, number-of-arguments, odd?, or, quotient, random, remainder, round, sentence, sqrt, vowel?, and word. 
 
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
|member?|the|#t|*(the best language is scheme)*|
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


