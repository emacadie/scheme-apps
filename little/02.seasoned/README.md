Code for the exercises in "The Seasoned Schemer", using the R6RS packages in Racket.    

https://github.com/pkrumins/the-seasoned-schemer

https://github.com/viswanathgs/The-Seasoned-Schemer

http://community.schemewiki.org/?little-schemer

http://community.schemewiki.org/?seasoned-schemer

https://docs.racket-lang.org/r6rs/index.html

The Eleventh Commandment: Use additional arguments when a function needs to know what other arguments to the function have been like so far.   

The Twelfth Commandment: Use (letrec ...) to remove arguments that do not change for recursive applications.

The Thirteenth Commandment: Use (letrec ...) to hide and protect functions

The Fourteenth Commandment: Use (letcc ... ) to return values abruptly and promptly

The Fifteenth Commandment: Use (let ...) to name the values of repeated
expressions in a function definition if they may be evaluated twice
for one and the same use of the function

The Sixteeth Commandment: Use (set! ... ) only with names defined in (let ... ) blocks

The Seventeenth Commandment (first version): Use (set! x ...) for (let ((x ...)) ... )
only if there is at least one (lambda ... ) between it and the "let"

(Note: this does not apply if you are defining functions with the name and args in a separate parens)

The Seventeenth Commandment (final version): 
Use (set! x ...) for (let ((x ...)) ... )
only if there is at least one (lambda ... ) between it and the "let"
or if the new value for x is a function that refers to x

The Eighteenth Commandment: Use (set! x ...) only when the value that x
refers to is no longer needed.

The Nineteenth Commandment: Use (set! ... ) to remember valuable things
between two distinct uses of a function


