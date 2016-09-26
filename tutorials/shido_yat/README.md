Yet Another Scheme Tutorial  
Hosted at http://www.shido.info/lisp/idx_scm_e.html   

Code to display multiple args from http://stackoverflow.com/questions/26539585/how-to-display-multiple-parameters-in-r5rs-scheme 
```scheme
(define (display-all . vs)
  (for-each display vs)
  (newline))
```
Call like this:   
```scheme
(display-all "n1: " n1 ", p:" p)
```
Not like this:   
```scheme
(display-all '("n1: " n1 ", p:" p))
```

