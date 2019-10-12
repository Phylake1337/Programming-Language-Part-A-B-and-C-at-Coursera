#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(define ones (lambda () (cons 1 ones)))

;Problem 1
(define (sequence low high stride)
  (if (> low high )
      null
      (cons low (sequence (+ low stride) high stride))))

;Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car(list-tail xs (remainder n (length xs))))]
      ))

;Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0) null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;Problem 5
(define funny-number-stream
  (letrec
      ([f (lambda(x) (if (= 0 (remainder x 5))
                        (cons (- 0 x) (lambda () (f (+ x 1))))
                        (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;Problem 6
(define dan-then-dog
  (letrec ([f (lambda (string)
                (if (string=? string "dan.jpg") (cons "dog.jpg" (lambda () (f "dog.jpg")))
                    (cons "dan.jpg" (lambda () (f "dan.jpg")))))])
    (lambda () (f "dog.jpg"))))


;Problem 7
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car(s))) (stream-add-zero (cdr(s))))))

;Problem 8
(define (cycle-lists xs ys)
  (letrec([help (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda ()(help (+ n 1)))))])
          (lambda () (help 0))))
;Problem 9
(define (vector-assoc v vec)
  (letrec([help (lambda (n)
                  (if (< n (vector-length vec))
                      (if (pair? (vector-ref vec n))
                          (if (equal? (car(vector-ref vec n)) v)
                              (vector-ref vec n)
                              (help (+ n 1)))
                          (help (+ n 1)))
                      #f ))])
          (help 0)))

;Problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [index 0])
    (lambda (v)
      (if (vector-assoc v cache)
          (vector-assoc v cache)
          (let ([cur-val (assoc v xs)])
            (begin
              (vector-set! cache index cur-val)
              (set! index
                    (remainder (+ index 1) n))
              (vector-assoc v cache)))))))