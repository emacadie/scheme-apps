Now trying Simply Scheme with Racket, on the command line

repo for "Simply Scheme": https://people.eecs.berkeley.edu/~bh/ss-toc2.html   

A big part of the reason is that I want to be able to run some tests.  
I can run tests in Clojure, but Scheme is not as good in this area.  

ttt.rkt and spread.rkt and changed somewhat to work with Racket mode. Original Scheme files downloaded from https://people.eecs.berkeley.edu/~bh/downloads/simply/ I could not find links to them on the "Simply Scheme" site. I found them through a Google search.

"dddbmt", "grades", and "r5rs" from Chapter 22 of the text.   

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


raco pkg install drracket  
ericm@latitude:~/Downloads$ raco pkg show  
Installation-wide:  
 Package            Checksum             Source  
 main-distribution  59e9a33d2c5f01ae...  catalog...tribution  
 racket-lib         cd090dc3c3452a44...  catalog racket-lib  
 [188 auto-installed packages not shown]  
User-specific for installation "6.9":  
 Package                     Checksum          Source  
 planet-dyoo-simply-scheme2  ce6e47b87642f...  catalog...cheme2  
 simply-scheme               8b8ba2b50d868...  catalog...scheme  

I uninstalled the simply-scheme, and I think I installed it with this:  
raco pkg install simply-scheme  
Either that or I did it in Dr Racket. Don't remember.  
Using DrRacket version 6.9.  

https://www.hashcollision.org/simply-scheme/  



Emacs commands: racket-run  
https://github.com/greghendershott/racket-mode/blob/master/Reference.md   

What finally installed it:  
in init.el:  
Adding the URL for MELPA  
Adding "racket-mode" to the list in (defvar my-packages  
Adding   
;; Racket  
(setq racket-program "racket")  
and adding "racket-mode" to  
'(package-selected-packages  
   (quote  
at the end. Then emacs downloaded it all for me.  

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

Format later:
application: not a procedure;                                                                                                                                        
expected a procedure that can be applied to arguments                                                                                                               
given: 1                                                                                                                                                           
arguments...: [none] 
I used (screen-num), which is a symbol, not a procedure
