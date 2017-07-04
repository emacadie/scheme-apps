A repo for things I do in Scheme.  

For now, just some tutorials.  

So far, I like Chez Scheme the best on Ubuntu. It does indentation and parentheses matching in the REPL.   
And I can cntl-K and cntl-Y like in emacs.  

R6RS procedures/functions:  
http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-21.html#node_index_start  
http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-21.html#node_index_start  

R7RS functions:  
http://justinethier.github.io/husk-scheme/manual/node106.html   

JEdit regex to clean up kawa prompt when I copy/paste: .*kawa:\d{1,}\|\#  then \s\n or \#\|kawa:\d{1,}\|\#\s    

Chicken Scheme interpreter with R7RS: csi -R r7rs    
Or run csi and then type this in REPL:   
```scheme
 (require-extension r7rs)  
```



Which Chicken eggs are installed: chicken-status -eggs  
Install an egg: chicken-install EXTENSIONNAME 

To get Scheme to work in emacs, add this to init.el:   
```
(setq scheme-program-name "csi -:c")
```
Then, to work with Scheme, c-x 2 to get another window, c-x o to go to the second one, and
```
M-x run-scheme
```
To get a REPL.   

Directions from http://community.schemewiki.org/?emacs-tutorial:  
C-x C-e: Send the last sexpr to your Scheme process  (note to self: put cursor after last paren)
C-x h C-c C-r: Send the whole buffer. This first marks (cf. the tutorial) the whole buffer (C-x h), and then sends the region (C-c C-r). If you can't seem to remember C-x h like the present author, you can use M-< M-> as well, which moves to the beginning of the buffer, and then to the end. 

!! Note that these commands assume your key bindings for :C-x C-e: and :C-x h C-c C-r: are correct!! 
-- end quote


