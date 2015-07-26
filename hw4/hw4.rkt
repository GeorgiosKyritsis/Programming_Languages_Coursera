#lang racket

;1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;Test
(define x11 (sequence 3 11 2))
(define x12 (sequence 3 8 3))
(define x13 (sequence 3 2 1))

;2
(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs))

;Test
(define x21 (string-append-map (list "a" "B" "c") "wer"))

;3
(define (list-nth-mod xs n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))

;Test
(define x31 (list-nth-mod (list 1 2 3 4 5 6 7) 13))

;Helper stream
(define powers-of-two
  (letrec
      ([f (lambda(x) (cons x (lambda() (f (* x 2)))))])
    (lambda() (f 2))))

;4
(define (stream-for-n-steps s n)
  (cond 
    [(= n 0) null]
    [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

;Test
(define x41 (stream-for-n-steps powers-of-two 20))

;5
(define funny-number-stream
  (letrec
      ([f (lambda(x) (cons (if (= (remainder x 5) 0) (- x) x) (lambda() (f (+ x 1)))))])
    (lambda() (f 1))))

;Test
(define y51 (stream-for-n-steps funny-number-stream 20))

;6
(define dan-then-dog
  (letrec
      ([f (lambda(x) (cons x (lambda() (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda() (f "dan.jpg"))))

;Test
(define y61 (stream-for-n-steps dan-then-dog 5))

;7
(define (stream-add-zero s)
  (letrec
      ([f (lambda(x) (cons (cons 0 (car (x))) (stream-add-zero (cdr (x)))))])
    (lambda() (f s))))

;Test
(define y71 (stream-for-n-steps (stream-add-zero powers-of-two) 10))

;8
(define (cycle-lists xs ys)
  (letrec
      ([f (lambda(n) 
            (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda() (f (+ n 1)))))])
    (lambda() (f 0))))

;Test
(define y81 (cycle-lists (list 1 2 3) (list "a" "b")))
(define y82 (stream-for-n-steps y81 10))

;9
(define (vector-assoc v vec)
  (letrec
      ([len (vector-length vec)]
       [f (lambda(n)
            (cond
              [(= n len) #f]
              [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
              [#t (f (+ n 1))]))])
    (f 0)))

;Test
(define y91 (list->vector (cons (cons 1 "a") (cons (cons 2 "b") null))))
(define y92 (vector-assoc 1 y91))

;10
(define (cached-assoc xs n)
  (letrec
      ([cache (make-vector n #f)]
       [index 0]
       [f (lambda(v)
            (let
                ([ans (vector-assoc v cache)])
              (if ans
                  ans
                  (let
                    ([new-ans (assoc v xs)])
                    (if new-ans
                        (begin (vector-set! cache index new-ans)
                                  (set! index (remainder (+ index 1) n))
                                  new-ans)
                        #f)))))])
    f))


