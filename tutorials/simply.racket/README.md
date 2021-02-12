Now trying Simply Scheme with Racket, on the command line

repo for "Simply Scheme": https://people.eecs.berkeley.edu/~bh/ss-toc2.html   

A big part of the reason is that I want to be able to run some tests.  
I can run tests in Clojure, but Scheme is not as good in this area.  

Implementations of map, filter, reduce in chapter19.rkt: my-map, my-keep, my-reduce    

https://github.com/greghendershott/racket-mode  
https://melpa.org/#/racket-mode  

Racket Mode website: https://www.racket-mode.com/  

https://docs.racket-lang.org/manual@simply-scheme/index.html  
https://pkgs.racket-lang.org/package/simply-scheme  

Racket reference:  
https://docs.racket-lang.org/reference/index.html   

Racket Cheat Sheet:   
https://docs.racket-lang.org/racket-cheat/index.html   

Racket Unit Testing:  
https://docs.racket-lang.org/rackunit/index.html  

Racket SRFIs:   
https://docs.racket-lang.org/srfi/   



Emacs commands: racket-run  
https://github.com/greghendershott/racket-mode/blob/master/Reference.md   


You may have to M-x racket-mode as well.  
To get repl: M-x racket-run  
To run the tests:  
M-x racket-test  

RACKET_HOME=$HOME/racket/bin
PATH=$MAVEN_HOME/bin:$JAVA_HOME/bin:/zEKM/android-sdk-linux/tools:$RACKET_HOME:$ANT_HOME/bin:$PATH

```batchfile
RACKET_HOME=/cygdrive/c/'Program Files'/Racket
PATH=$RACKET_HOME:$PATH
```

Or try this:
```batchfile
set RACKET_HOME=C:\Program Files\Racket
PATH=%RACKET_HOME%;%PATH%
```

To put multiple expressions in a conditional/if branch: Use begin, not do   
https://stackoverflow.com/questions/11263359/is-there-a-possibility-of-multiple-statements-inside-a-conditional-statements-b

Implementations of my-map, my-keep, my-reduce in chapter19.rkt

