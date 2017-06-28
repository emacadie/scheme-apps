This chapter is about procedures.   

Procedures in Scheme have four parts: the word "define", the name, the parameters, the body.   

A "function" is what a procedure does, "process" is how it does it. Take these two:  
```
(define (f x)
  (+ (* 3 x) 12))

(define (g x)
  (* 3 (+ x 4)))
```
They give the same result, so they perform/represent the same function, but they do it differently, so they are different procedures. I think this is a slightly different definition of "function", but then I am not a mathematician.   

Parameter vs. argument: A procedure definition has parameters; a specific call has arguments.   

Note: Scheme does not have an "average" function. Probably because you can average more than two values.  

```
(define (average a b)
  (/ (+ a b) 2))
```   

Composability: the result of a procedure can be the argument to the next procedure.    

Two pitfalls: procedures can only return one value, and be careful with name collision. I know the Scheme people want their language to be simple, but perhaps it is time for namespaces.   

  

