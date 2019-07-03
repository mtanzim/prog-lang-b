
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define ones (lambda () (cons 1 ones)))
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define (funny-helper x)
  (if (= (remainder x 5) 0)
      (* -1 x)
      x))

(define (stream-maker fn arg helper)
  (letrec ([f (lambda (x) 
                (cons (helper x) (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))

(define funny-number-stream (stream-maker + 1 funny-helper))

(define powers2 (stream-maker * 2 (lambda (x) x) ))


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

(define (stream-for-n-steps s n)
  (define (helper cur-xs cur-count cur-s)
    (if (= cur-count n)
        cur-xs
        (helper (append cur-xs (list(car (cur-s))) ) (+ 1 cur-count)  (cdr (cur-s))  )))
  (helper  '() 0 s))