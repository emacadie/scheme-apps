repo for "Simply Scheme": https://people.eecs.berkeley.edu/~bh/ss-toc2.html  

Why? It claims to be a prequel to SICP   

I think their code is GPL. I have tried to make a few small changes to get it to work in Kawa. But I don't think it's worth it to get it to work in Kawa. It does work with csi and guile.   

database.scm, functions.scm, match.scm, newttt.scm, simply.scm, spread.scm and ttt.scm from https://people.eecs.berkeley.edu/~bh/downloads/simply/ I could not find links to them on the "Simply Scheme" site. I found them through a Google search.   

You load simply.scm first    
(load-relative "./simply.scm")   
(load-relative "./functions.scm")   
  
Chapter two:   

|function|arg 1|arg 2|result|
|--- | --- |--- |--- |
|word|now|here|*nowhere*|
 sentence 	 now	 here
 first	 blackbird	 none
 first	 (blackbird) 	 none
	 3	 4	 7
 every		 (thank you girl) 	 (hank ou irl) 
 member?	 e	 aardvark
 member?	 the		 #t
 keep	 vowel?	 (i will)
 keep	 vowel?		 eieio[5]
 last	 ()	 none	
	 last	 (honey pie)	 (y e)
		 taxman	 aa
