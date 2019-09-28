;; PL Project - Fall 2018
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus  (e1 e2)  #:transparent)  ;; struct two expressions
(struct mult  (e1 e2)  #:transparent)  ;; multiply two expressions
(struct div  (e1 e2)  #:transparent)  ;; div two expression
(struct neg  (e)  #:transparent)  ;; div two expression

(struct bool  (b)  #:transparent)  ;; div two expression
(struct andalso  (b1 b2)  #:transparent)  ;; div two expression
(struct orelse  (b1 b2)  #:transparent)  ;; div two expression

(struct cnd  (e1 e2 e3)  #:transparent)  ;; div two expression
(struct iseq  (e1 e2)  #:transparent)  ;; div two expression
(struct ifnzero  (e1 e2 e3)  #:transparent)  ;; div two expression
(struct ifleq  (e1 e2 e3 e4)  #:transparent)  ;; div two expression

(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application


(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent)

;;user defined structs
(struct apair (e1 e2) #:transparent)
(struct 1st (e1) #:transparent)
(struct 2nd (e1) #:transparent)

(struct with  (s e1 e2)  #:transparent)  ;; div two expression


;; Problem 1

(define (racketlist->numexlist xs)
  (cond
    [(null? xs) munit]
    [(list? xs) (apair (car xs)(racketlist->numexlist (cdr xs)))]
    [#t (error "parameter given is not a raket list")]
   )
  )
(define (numexlist->racketlist xs)
  (cond
    [(munit? xs) munit]
    [(list? xs) (cons (1st xs)(numexlist->racketlist (2nd xs)))]
    [#t (error "parameter given is not a numx list")]
   )
  )

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable:" str)]
        [(equal? str (car (car env))) (cdr (car env))]
         [else (envlookup (cdr env) str)]
  )
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(munit? e) e]
        [(closure? e) e]
        [(num? e)
         (cond [(number? (num-int e)) e]
               [else (error "NUMEX num applied to non-number")])
         ]

        [(bool? e)
         (cond [(boolean? (bool-b e)) e]
               [else (error "NUMEX bool applied to non-boolean")])
         ]

        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX minus applied to non-number")))]
        
        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX mult applied to non-number")))]

         [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (quotient (num-int v1) 
                       (num-int v2)))
               (error "NUMEX div applied to non-number")))]

         [(neg? e) 
         (let ([v (eval-under-env (neg-e e) env)])
           (if (num? v)    
               (num (- (num-int v)))
               (error "NUMEX neg applied to non-number")))]

         [(andalso? e) 
         (let ([v1 (eval-under-env (andalso-b1 e) env)]
               [v2 (eval-under-env (andalso-b2 e) env)])
           (if (and (bool? v1) (not (bool-b v1)))
               (bool #f)
               (if (and (bool? v1)
                    (bool? v2))
                   (bool (and (bool-b v1) (bool-b v2)))
                   (error "NUMEX andalso applied to non-boolean")))
                   )]

         [(orelse? e) 
         (let ([v1 (eval-under-env (orelse-b1 e) env)]
               [v2 (eval-under-env (orelse-b2 e) env)])
           (if (and (bool? v1) (bool-b v1))
               (bool #t)
               (if (and (bool? v1) (bool? v2))
                   (bool (or (bool-b v1) (bool-b v2)))
                   (error "NUMEX andalso applied to non-boolean")))
                   )]

         [(cnd? e)
          (let ([v (eval-under-env (cnd-e1 e) env)])
            (if (bool? v)
                (if (and (bool-b v) #t)
                  (eval-under-env (cnd-e2 e) env)
                  (eval-under-env (cnd-e3 e) env))
                (error "NUMEX cnd applied to non-boolean")))]

         [(iseq? e) 
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (cond 
                [(and (num? v1)(num? v2))
                 (bool (= (num-int v1) 
                          (num-int v2)))]
                [(and (bool? v1)(bool? v2))
                 (bool (not (xor (bool-b v1) 
                          (bool-b v2))))]        
               [#t (error "NUMEX iseq: first and second arguments are not same type")]))]

          [(ifnzero? e)
          (let ([v (eval-under-env (ifnzero-e1 e) env)])
            (if (num? v)
                (if (= (num-int v) 0)
                  (eval-under-env (ifnzero-e2 e) env)
                  (eval-under-env (ifnzero-e3 e) env))
                (error "NUMEX ifnzero: first argument is not a number")))]
         [(ifleq? e)
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (if (> (num-int v1) (num-int v2))
                   (eval-under-env (ifleq-e4 e) env)
                   (eval-under-env (ifleq-e3 e) env))
               (error "NUMEX ifleq applied to non-number")))]
         
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
                (apair v1 v2))]

        [(1st? e)
         (let ([v (eval-under-env (1st-e1 e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "NUMEX 1st applied to non-apair" e)
               ))]

        [(2nd? e)
         (let ([v (eval-under-env (2nd-e1 e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "NUMEX 2nd applied to non-apair")))]
        
        [(ismunit? e)
         (let ([v (eval-under-env (ismunit-e e) env)])
            (if (munit? v)
                (bool #t)
                (bool #f)))]
         [(with? e)
          (let ([v1 (eval-under-env (with-e1 e) env)]
                [s (with-s e)])
             (if (string? s)
                 (eval-under-env (with-e2 e) (append env (list (cons s v1))))
                 (error "NUMEX with expected string")))
          ]
         [(lam? e)
          (if (and
               (string? (lam-formal e))
               (or
                (string? (lam-nameopt e))
                (null? (lam-nameopt e))))
              (closure env e)
              (error "NUMEX lam : first and second parameters must be string"))]
         [(apply? e)
         (let ([value (eval-under-env (apply-actual e) env)]
               [function (eval-under-env (apply-funexp e) env)])
           (if (closure? function)
               (let ([closureFunction (closure-f function)])
                 (if (null? (lam-nameopt closureFunction))
                     (eval-under-env
                      (lam-body closureFunction)
                      (cons (cons (lam-formal closureFunction) value) (closure-env function)))
                     (eval-under-env
                      (lam-body closureFunction)
                      (cons (cons (lam-nameopt closureFunction) function)
                            (cons (cons (lam-formal function) value) (closure-env function))))))
               (error "NUMEX call applied to non-function" e)))]
         
        
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifmunit e1 e2 e3)
   (cnd (ismunit e1) e2 e3))

(define (with* bs e2)
  (if (null? bs)
      (with "null" (munit) e2)
      (with (car(car bs)) (with* (cdr(car bs)) e2))))

(define (ifneq e1 e2 e3 e4)
  (with "_x" e1
        (with "_y" e2
        (ifleq (var "_y") (var "_x")
               (ifleq (var "_x") (var "_y") e4 e3) e3))))
;; Problem 4

(define numex-filter
  (lam null "input-fun"
  (lam "mappper" "inputs"
       (cnd (ismunit (var "inputs"))
            (apair (apply (var "input-fun") (1st (var "inputs")))
                   (apply (var "mapper") (2nd (var "inputs"))))
            (munit)))))

(define numex-all-gt
  (with "filter" numex-filter
        (lam null "i"
             (apply (var "filter")
                    (lam null "xL"
                         (ifleq (var "xL") (var "i") (2nd (var "xL"))
                                (cons (var "xL") (apply (var "filter") (2nd (var "xL"))))))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

;;(eval-exp (with* (list (cons "hjgf" (num 12)))
;;                 (plus (var "a") (num 3))))



;;(eval-exp (bool 12))
;;(eval-exp (andalso(bool #t) (bool #t)))

;;(eval-exp (cnd (andalso(bool #t) (bool #t))
;;            (plus (num 1) (num 2))
;;            (plus (num 1) (num 3))
;;          ))

;;(eval-exp (iseq (bool #f) (bool #f)))
;;(eval-exp (ifnzero (num 0) (bool #f) (bool #t) ))
;;(eval-exp (ifleq (num 1) (num 0) (bool #t) (bool #f) ))
;;(eval-exp (2nd (apair (num 1) (apair(num 2) (num 3)))))
;;(eval-exp (ifneq (num 5) (num 3) (num 4) (num 5)))
;;(eval-exp (with "a" (num 12) (lam "a" "b" null)))
(eval-exp (orelse (bool #f) (bool #t)))
