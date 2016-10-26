;; 12 Symbols

(eq? (string->symbol "Hello") 'Hello) ;; #t On some scheme systems these are not equal
(eq? (string->symbol "Hello") (string->symbol "Hello")) ;; #t
(symbol->string (string->symbol "Hello")) ;; Hello

;; There is a long program which counts the words in a piece of text


