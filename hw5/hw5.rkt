;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;;; (define (racketlist->mupllist rkt-lst)
  ;;; (define (f cur-rkt-list cur-list)
    ;;; (if (null? cur-rkt-list)
        ;;; cur-list
        ;;; (f (cdr cur-rkt-list) (apair (car cur-rkt-list) cur-list))))
  ;;; (f (reverse rkt-lst) (aunit)) )

(define (racketlist->mupllist rkt-lst)
  (cond [(null? rkt-lst) (aunit)]
        [(list? rkt-lst) (apair (car rkt-lst) (racketlist->mupllist (cdr rkt-lst)))]
        [#t "error"]))

(define (mupllist->racketlist mupl-lst)
  (cond [(aunit? mupl-lst) null]
        [(apair? mupl-lst) (cons (apair-e1 mupl-lst) (mupllist->racketlist (apair-e2 mupl-lst)))]
        [#t "error"]
        ))

;; Problem 2
;; lookup a variable in an environment
;; Do NOT change this function
;; env is a racket list[str, MUPL-val]
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(fun? e) e]
        [(aunit? e) (aunit)]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           ;(writeln v1)
           ;(writeln v2)
           (apair v1 v2))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           ;(writeln v1)
           (if (apair? v1)
               (apair-e1 v1)
               (error "Not a pair")))]
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           ;(writeln v1)
           (if (aunit? v1)
               (int 1)
               (int 0)))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           ;(writeln v1)
           (if (apair? v1)
               (apair-e2 v1)
               (error "Not a pair")))]
        [(mlet? e)
         (letrec ([cur-var (mlet-var e)]
               [var-val (eval-under-env (mlet-e e) env)]
               [cur-env (cons (cons cur-var var-val) env )]
               )
           ;(writeln cur-var)
           ;(writeln var-val)
           ;(writeln cur-env)
           (eval-under-env (mlet-body e) cur-env))]
        [(call? e)
         (if (closure? (call-funexp e))
             (letrec ([cur-fun-name (fun-nameopt (closure-fun (call-funexp e)))]
                      [cur-fun-arg-name (fun-formal (closure-fun (call-funexp e)))]
                      [cur-fun-arg-val (eval-under-env (call-actual e) env)]
                      [cur-fun-body (fun-body (closure-fun (call-funexp e)))]
                      [start-fun-env (cons (cons cur-fun-arg-name cur-fun-arg-val) (closure-env (call-funexp e)))]
                      [ext-fun-env (if (not cur-fun-name )
                                       start-fun-env
                                       (append start-fun-env (cons cur-fun-name (closure-fun (call-funexp e)))  )
                                       )])
               ;(writeln cur-fun-name)
               ;(writeln cur-fun-arg-name)
               ;(writeln cur-fun-arg-val)
               ;(writeln cur-fun-body)
               ;(writeln start-fun-env)
               ;(writeln ext-fun-env)
               (eval-under-env cur-fun-body ext-fun-env))
             (error "Not a closure!"))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
              [v2 (eval-under-env (ifgreater-e2 e) env)])
              ;(writeln v1)
              ;(writeln v2)
              (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                    (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL if-greater applied to non-number")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (aunit? e1) e2 e3))

(define (mlet* lstlst e2)
  (define (iter-lst cur-lst cur-env)
    (writeln cur-env)
    (if (null? cur-lst)
        cur-env
        (iter-lst (cdr cur-lst) (cons cur-env (cons  (car (car cur-lst)) (cdr (car cur-lst))) ))))
  (iter-lst lstlst null)

  )
    

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
