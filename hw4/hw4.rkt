
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
    (if (> low high) 
        null
        (cons low (sequence (+ low stride) high stride))
    )
)
(define (string-append-map xs suffix)
    (map (lambda (i) (string-append i suffix)) xs)
)

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
            (helper xs 0)))
)