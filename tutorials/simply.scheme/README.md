repo for "Simply Scheme": https://people.eecs.berkeley.edu/~bh/ss-toc2.html  

Why? It claims to be a prequel to SICP   

I think their code is GPL. I have tried to make a few small changes to get it to work in Kawa. But I don't think it's worth it to get it to work in Kawa. It does work with csi and guile.   

database.scm, functions.scm, match.scm, newttt.scm, simply.scm, spread.scm and ttt.scm from https://people.eecs.berkeley.edu/~bh/downloads/simply/ I could not find links to them on the "Simply Scheme" site. I found them through a Google search.   

You load simply.scm first    

(load-relative "./simply.scm")   
(load-relative "./functions.scm")
Or:   
(load-relative "/home/ericm/github/scheme-apps/tutorials/simply.scheme/simply.scm")   
(load-relative "/home/ericm/github/scheme-apps/tutorials/simply.scheme/functions.scm")   
(load-relative "/home/ericm/github/scheme-apps/tutorials/simply.scheme/more.functions.scm")   
(load-relative "/home/ericm/github/scheme-apps/tutorials/simply.scheme/ttt.scm")   
For guile, just use "load"  

2018-12-24: For some reason, this does not seem to work in Guile anymore.   

Here is a Racket module for Simply Scheme: http://www.hashcollision.org/simply-scheme/   
https://github.com/dyoo/simply-scheme  

One guy's solutions: https://github.com/buntine/Simply-Scheme-Exercises   

https://github.com/pongsh/simply-scheme-exercises   
(Neither have the answer to the "hotshot" question in 4.8)   

My solutions are intermingled with notes that I take along the way.    

To use in Racket:   
If not installed: raco pkg install simply-scheme  
http://docs.racket-lang.org/manual@simply-scheme/index.html  
https://pkgs.racket-lang.org/package/simply-scheme  
https://github.com/jbclements/simply-scheme   
You have to hit the "Run" arrow after you define your function. The only thing I do not like is that is clears your REPL window.   



