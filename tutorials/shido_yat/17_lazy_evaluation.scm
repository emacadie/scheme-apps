;; 17 Lazy Evaluation

(define laz (delay (+ 1 2)))
laz ;; 3
(promise? laz) ;; #t
(force laz) ;; 3
(* 10 (force laz)) ;; 30
;; you can call "force" multiple times, meaning you can re-use your promise multiple times

