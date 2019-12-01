Code for the exercises in "The Little Schemer", using the R6RS packages in Racket.    

From Chapter 1:   
The Law of Car: The primitive car is defined only for non-empty lists.   
The Law of Cdr: The primitive cdr is defined only for non-empty lists.    
The cdr of any non-empty list is always another list.     
The Law of Cons: The primitive cons takes two arguments.     
The second argument to cons must be a list.    
The result is a list.    
The Law of Null?: The primitive null? is defined only for lists.        
The Law of Eq? The primitive eq? takes two arguments. Each must be a non-numeric atom.      

The First Commandment: Always ask null? as the first question in expressing any function.  
The Second Commandment: Use cons to build lists.   
The Third Commandment: When building a list, describe the first typical element, and then cons it onto the natural recursion.      
The Fourth Commandment (preliminary): Always change at least one argument while recurring.    
It must be changed to be closer to termination.    
The changing argument must be tested in the termination condition: when using cdr, test termination with null?.   
   
The Fifth Commandment:   
When building a value with +, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition.    
When building a value with x (multiplication), always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication.    
When building a value with cons, always consider '() for the value of the terminating line.    



Can use rb6: prefix:  
* eq?  
* equal?  
* list?  
* null?  
* or
* quote  
* zero?

Does not work with rb6 prefix:
* car
* cdr


