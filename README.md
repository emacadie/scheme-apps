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


