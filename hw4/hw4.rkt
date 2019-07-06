
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file


;; helper functions
(define ones (lambda () (cons 1 ones)))
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))


;; put your code below
(define (sequence low high stride)
    (if (> low high) 
        null
        (cons low (sequence (+ low stride) high stride))
    ))
(define (string-append-map xs suffix)
    (map (lambda (i) (string-append i suffix)) xs))

(define (list-nth-mod xs num)
  (define (helper cur-xs cur-count)
            (if (null? cur-xs)
                (error "list-nth-mod: not found")
                (if (= (remainder num (length xs)) cur-count)
                    (car cur-xs)
                    (helper (cdr cur-xs) (+ cur-count 1)))))
  (if (= (length xs) 0)
      (error "list-nth-mod: empty list")
      (if (< num 0)
          (error "list-nth-mod: negative number")
          (helper xs 0))))

(define (stream-maker fn arg helper)
  (letrec ([f (lambda (x) 
                (cons (helper x) (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))
;(define powers2 (stream-maker * 2 (lambda (x) x) ))

(define (stream-for-n-steps s n)
  (define (helper cur-xs cur-count cur-s)
    (if (= cur-count n)
        cur-xs
        (helper (append cur-xs (list(car (cur-s))) ) (+ 1 cur-count)  (cdr (cur-s))  )))
  (helper  '() 0 s))


(define funny-number-stream (stream-maker + 1 (lambda (x)
  (if (= (remainder x 5) 0)
      (* -1 x)
      x))))

(define dan-then-dog (stream-maker + 1 (lambda (x)
  (if (= (remainder x 2) 0)
      "dog.jpg"
      "dan.jpg"))))

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) 
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda () (f (+ 1 x)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (define (helper cur-i)
    (if (= cur-i (vector-length vec))
        #f
        (begin
          (if (and (pair? (vector-ref vec cur-i)) (equal? (car (vector-ref vec cur-i)) v) )
              (vector-ref  vec cur-i)
              (helper (+ cur-i 1))))))
  (helper 0))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [cur-pos 0]
  [f (lambda (v)
       (if (not (vector-assoc v memo))
           (let ([new-ans (assoc v xs)])
             (begin
               (if new-ans
                   (begin
                     (vector-set! memo cur-pos new-ans)
                     (if (= cur-pos n)
                         (set! cur-pos (+ 1 cur-pos))
                         (set! cur-pos 0))
                     ;(writeln cur-pos)
                     ;(writeln memo)
                     new-ans
                     )
                   (begin
                     new-ans))
             ))
           (vector-assoc v memo))
       )])
  f
    ))
  
        
  
