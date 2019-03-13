Now trying Simply Scheme with Racket, on the command line

A big part of the reason is that I want to be able to run some tests.  
I can run tests in Clojure, but Scheme is not as good in this area.  


https://github.com/greghendershott/racket-mode  
https://melpa.org/#/racket-mode  

https://docs.racket-lang.org/manual@simply-scheme/index.html  
https://pkgs.racket-lang.org/package/simply-scheme  

Racket reference:  
https://docs.racket-lang.org/reference/index.html   

Racket Cheat Sheet:   
https://docs.racket-lang.org/racket-cheat/index.html   

Racket Unit Testing:  
https://docs.racket-lang.org/rackunit/index.html  

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

