repo for "Simply Scheme": https://people.eecs.berkeley.edu/~bh/ss-toc2.html  

Why? It claims to be a prequel to SICP   

I think their code is GPL. I have tried to make a few small changes to get it to work in Kawa. But I don't think it's worth it to get it to work in Kawa. It does work with csi and guile.   

database.scm, functions.scm, match.scm, newttt.scm, simply.scm, spread.scm and ttt.scm from https://people.eecs.berkeley.edu/~bh/downloads/simply/ I could not find links to them on the "Simply Scheme" site. I found them through a Google search.   

You load simply.scm first    
(load-relative "./simply.scm")   
(load-relative "./functions.scm")   
  
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
