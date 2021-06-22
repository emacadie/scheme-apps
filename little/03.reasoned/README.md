Repo for The Reasoned Schemer

https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd  
The official repo  

https://github.com/pkrumins/the-reasoned-schemer  
https://github.com/hkoktay/the-reasoned-schemer  
https://github.com/miniKanren/TheReasonedSchemer - One of the authors  
There is this: https://docs.racket-lang.org/minikanren/index.html  
https://github.com/miniKanren/Racket-miniKanren  
https://github.com/philoskim/reasoned-schemer-for-clojure  
https://github.com/klutometis/reasoned-schemer - notes in org-mode  
https://github.com/cjsauer/reasoned-schemer - also in Clojure  
https://github.com/candera/reasoned-schemer - again in Clojure    
https://github.com/martintrojer/reasoned-schemer-core.logic - and again in Clojure    
Do people hate Racket/Scheme?

http://minikanren.org/

https://www.monolune.com/using-racket-for-the-reasoned-schemer/

https://github.com/takikawa/minikanren/  
Says to use 
https://github.com/calvis/cKanren   
https://pkgs.racket-lang.org/package/cKanren    
Look at stuff in https://github.com/calvis/cKanren/blob/master/cKanren/miniKanren.rkt


Canonical Racket repo (not in Racket packages):   
https://github.com/miniKanren/Racket-miniKanren    

Local notes for minikanren:
file:///home/ericm/.racket/6.9/pkgs/minikanren/minikanren/doc/minikanren/index.html  
Local notes for cKanren:  
None  

Preface: Starting with two constants: #s and #u  (like #t and #f)   
Three operators: an equal sign w/three lines, fresh (like lambda) and cond^e (like cond)  
"A relation, a function that returns a goal as its value, ends its name with a superscript 'o'"  
cKanren seems to use succeed and fail in for #s and #u


The Law of Fresh: If x is fresh, then (≡ v x) succeeds and associates x with v   
The Law of ≡: (≡ v w) is the same as (≡ w v)   


