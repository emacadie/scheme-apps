;; 09 structs
;; structures

;; this works in chez scheme v9
(define-structure (tree height girth age leaf-shape leaf-color))
;; chez scheme wants all fields in make-tree
(define coconut
    (make-tree 30 ;; height 
        10 ;; girth 
        20 ;; age 
        'square ;; leaf-shape 
        'green)) ;; leaf-color
coconut ;; #(tree 30 10 20 square green)
(tree-height coconut) ;; 30
(tree-age coconut) ;; 20
(tree-leaf-color coconut) ;; green
(set-tree-age! coconut 44)
(tree-age coconut) ;; 44
;; chez scheme macro is at http://cisco.github.io/ChezScheme/csug9.4/compat.html#./compat:s25
;; to see what the macro gives you, use expand
(expand '(define-structure (tree height girth age leaf-shape leaf-color)))

;;;;;;;;;;;;;;;;;;;
;; their macro for structs is defstruct
;; it needs this function from chapter 6
;; note: This macro will not work in chez scheme
;; This works kawa and guile
(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l) #f
          (if (eqv? (car l) o) i
              (loop (+ i 1) (cdr l)))))))

(define-macro defstruct
  (lambda (s . ff)
    (let ((s-s (symbol->string s)) (n (length ff)))
      (let* ((n+1 (+ n 1))
             (vv (make-vector n+1)))
        (let loop ((i 1) (ff ff))
          (if (<= i n)
            (let ((f (car ff)))
              (vector-set! vv i 
                (if (pair? f) (cadr f) '(if #f #f)))
              (loop (+ i 1) (cdr ff)))))
        (let ((ff (map (lambda (f) (if (pair? f) (car f) f))
                       ff)))
          `(begin
             (define ,(string->symbol 
                       (string-append "make-" s-s))
               (lambda fvfv
                 (let ((st (make-vector ,n+1)) (ff ',ff))
                   (vector-set! st 0 ',s)
                   ,@(let loop ((i 1) (r '()))
                       (if (>= i n+1) r
                           (loop (+ i 1)
                                 (cons `(vector-set! st ,i 
                                          ,(vector-ref vv i))
                                       r))))
                   (let loop ((fvfv fvfv))
                     (if (not (null? fvfv))
                         (begin
                           (vector-set! st 
                               (+ (list-position (car fvfv) ff)
                                  1)
                             (cadr fvfv))
                           (loop (cddr fvfv)))))
                   st)))
             ,@(let loop ((i 1) (procs '()))
                 (if (>= i n+1) procs
                     (loop (+ i 1)
                           (let ((f (symbol->string
                                     (list-ref ff (- i 1)))))
                             (cons
                              `(define ,(string->symbol 
                                         (string-append
                                          s-s "." f))
                                 (lambda (x) (vector-ref x ,i)))
                              (cons
                               `(define ,(string->symbol
                                          (string-append 
                                           "set!" s-s "." f))
                                  (lambda (x v) 
                                    (vector-set! x ,i v)))
                               procs))))))
             (define ,(string->symbol (string-append s-s "?"))
               (lambda (x)
                 (and (vector? x)
                      (eqv? (vector-ref x 0) ',s))))))))))
;;
(defstruct tree height girth age leaf-shape leaf-color)
(define coconut
    (make-tree 'height 30
        'leaf-shape 'frond
        'age 5))
(tree.leaf-shape coconut) ;; frond
(tree.height coconut) ;; 30
(set!tree.height coconut 40)
(tree.height coconut) ;; 40

;; using their macro, supply some defaults
(defstruct tree height girth age
    (leaf-shape 'frond)
    (leaf-color 'green))
(define palm (make-tree 'height 60))
palm ;; #(tree 60   frond green)
(tree.height palm) ;; 60
(tree.leaf-shape palm) ;; frond

(define plantain
    (make-tree 'height 7
        'leaf-shape 'sheet))
(tree.height plantain) ;; 7
(tree.leaf-shape plantain) ;; sheet
(tree.leaf-color plantain) ;; green

;; to see what the macro gives you, use expand
(require 'syntax-utils)
(expand '(defstruct tree height girth age leaf-shape leaf-color))


