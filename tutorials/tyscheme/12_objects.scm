;; 12 Objects and classes

(defstruct standard-class
    slots superclass method-names method-vector)

(define-macro create-class
    (lambda (superclass slots . methods)
        `(create-class-proc
            ,superclass
            (list ,@(map (lambda (slot) `',slot) slots))
            (list ,@(map (lambda (method) `',(car method)) methods))
            (vector ,@(map (lambda (method) `,(cadr method)) methods)))))

(define make-instance
    (lambda (class . slot-value-twosomes)
        (display "making class ")
        (display class)
        (newline)
        ; find 'n', the number of slots in 'class'
        ; create an instance vector of length 'n + 1'
        ; because we need one extra element in the instance to contain the class
        
        (let* ((slotlist (standard-class.slots class))
                (n (length slotlist))
                (instance (make-vector (+ n 1))))
            (vector-set! instance 0 class)
            ;; fill each of the slots in the instance with the value
            ;; as specified in teh call to 'make-instance'
            (let loop ((slot-value-twosomes slot-value-twosomes))
                (if (null? slot-value-twosomes) instance
                    (let ((k (list-position (car slot-value-twosomes)
                                    slotlist)))
                        (vector-set! instance (+ k 1)
                            (cadr slot-value-twosomes))
                        (loop (cddr slot-value-twosomes))))))))


(define trivial-bike-class
    (make-standard-class
        'superclass #t
        'slots '(frame parts size)
        'method-names '()
        'method-vector #()))

(define my-bike
    (make-instance trivial-bike-class
        'frame 'cromoly
        'size '18.5
        'parts 'alivio))

(define class-of
    (lambda (instance)
        (vector-ref instance 0)))

(define slot-value
    (lambda (instance slot)
        (let* ((class (class-of instance))
                (slot-index (list-position slot (standard-class.slots class))))
            (vector-ref instance (+ slot-index 1)))))

(define set!slot-value
    (lambda (instance slot new-val)
        (let* ((class (class-of instance))
                (slot-index (list-position slot (standard-class.slots class))))
            (vector-set! instance (+ slot-index 1) new-val))))

(define delete-duplicates
    (lambda (s)
        (if (null? s) s
            (let ((a (car s)) (d (cdr s)))
                (if (memv a d) (delete-duplicates d)
                    (cons a (delete-duplicates d)))))))

(define create-class-proc
    (lambda (superclass slots method-names method-vector)
        (make-standard-class
            'superclass superclass
            'slots
            (let ((superclass-slots
                        (if (not (eqv? superclass #t))
                            (standard-class.slots superclass)
                            '())))
                (if (null? superclass-slots) slots
                    (delete-duplicates
                        (append slots superclass-slots))))
            'method-names method-names
            'method-vector method-vector)))

;; you will call methods on objects with this function
(define send
  (lambda (method instance . args)
    (let ((proc
           (let loop ((class (class-of instance)))
             (if (eqv? class #t) (error 'send)
                 (let ((k (list-position 
                           method
                           (standard-class.method-names class))))
                   (if k
                       (vector-ref (standard-class.method-vector class) k)
                       (loop (standard-class.superclass class))))))))
      (apply proc instance args))))

(define bike-class
    (create-class
        #t
        (frame size parts chain tires)
        (check-fit (lambda (me inseam)
                (let ((bike-size (slot-value me 'size))
                        (ideal-size (* inseam 3/5)))
                    (let ((diff (- bike-size ideal-size)))
                        (cond ((<= -1 diff 1) 'perfect-fit)
                            ((<= -2 diff 2) 'fits-well)
                            ((< diff -2) 'too-small)
                            ((> diff 2) 'too-big))))))))


(define my-bike
    (make-instance bike-class
        'frame 'titanium
        'size 21
        'parts 'ultegra
        'chain 'sachs
        'tires 'continental))

;; to use check-fit method:
(send 'check-fit my-bike 32)

(define mtn-bike-class
    (create-class
        bike-class
        (suspension)
        (check-fit (lambda (me inseam)
                (let ((bike-size (slot-value me 'size))
                        (ideal-size (- (* inseam 3/5) 2)))
                    (let ((diff (- bike-size ideal-size)))
                        (cond ((<= -2 diff 2) 'perfect-fit)
                            ((<= -4 diff 4) 'fits-well)
                            ((< diff -4) 'too-small)
                            ((> diff 4) 'too-big))))))))

(define my-m-bike
    (make-instance mtn-bike-class
        'frame 'titanium
        'size 21
        'parts 'ultegra
        'chain 'sachs
        'tires 'continental
        'suspension 'good))

(slot-value my-m-bike 'suspension) ;; good
(send 'check-fit my-m-bike 24) ;; too-big

;; new oo-stuff
(define standard-class
    (vector 'value-of-standard-class-goes-here
        (list 'slots
            'class-precedence-list
            'method-names
            'methdo-vector)
        '()
        '(make-instance)
        (vector make-instance)))

;; (vector-set! vector k obj )
(vector-set! standard-class 0 standard-class)

(define-macro create-class
    (lambda (direct-superclasses slots . methods)
        `(create-class-proc
            (list ,@(map (lambda (su) `,su) direct-superclasses))
            (list ,@(map (lambda (slot) `',slot) slots))
            (list ,@(map (lambda (method) `',(car method)) methods))
            (vector ,@(map (lambda (method) `,(cadr method)) methods)))))

(define append-map
    (lambda (f s)
        (let loop((s s))
            (if (null? s) '()
                (append (f (car d))
                    (loop (cdr s)))))))

(define create-class-proc
    (lambda (direct-superclasses slots method-names method-vector)
        (let ((class-precedence-list
                    (delete-duplicates
                        (append-map
                            (lambda (c) (vector-ref c 2))
                            direct-superclasses))))
            (send 'make-instance standard-class
                'class-precedence-list class-precedence-list
                'slots
                (delete-duplicates 
                    (append slots (append-map
                            (lambda (c) (vector-ref c 1))
                            class-precedence-list)))
                'method-names method-names
                'method-vector method-vector))))

(define send
    (lambda (method-name instance . args)
        (let ((proc
                    (let ((class (class-of instance)))
                        (if (eqv? class #t) (error 'send)
                            (let loop ((class class)
                                    (superclasses (vector-ref class 2)))
                                (let ((k (list-position
                                                method-name
                                                (vector-ref class 3))))
                                    (cond (k (vector-ref
                                                (vector-ref class 4) k))
                                        ((null? superclasses) (error 'send))
                                        (else (loop (car superclasses)
                                                (cdr superclasses))))
                                    ))))))
            (apply proc instance args))))



