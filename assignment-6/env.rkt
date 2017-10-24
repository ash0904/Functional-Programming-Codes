
#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "ast.rkt" "op.rkt" "store.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;defining closure/rec-closure/functionHandle 


(define-datatype functionHandle functionHandle?
  [closure (formals (list-of symbol?)) (body Ast?) (env Env?)]
  [rec-closure (formals (list-of symbol?)) (body Ast?) (env Env?) (rec-tuples (list-of tuple?))])
 
(define closure? (lambda(x)
  (if (functionHandle? x)
    (cases functionHandle x
      [closure (formals body env) #t]
      (else #f))
   #f)))

(define rec-closure? (lambda(x)
  (if (functionHandle? x)
    (cases functionHandle x
      [rec-closure (formals body env tpls) #t]
      (else #f))
   #f)))
 

(define expressible? (or/c number? boolean? functionHandle?)) 
(define ans? expressible?)
(define reference? integer?)
(define denotable? reference?)
(define storable? expressible?)
(define denotable->expressible (lambda(thing) (*deRef thing)))
(define expressible->denotable (lambda(thing) (*newRef thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defining environment tuple
; tuple?:= [symbol? denotable?] => tuple?
(define tuple? (lambda(x)
                    (and (list? x) 
                         (symbol? (first x)) 
                         (denotable? (second x)))))

; mk-tuple:= [symbol? denotable?] => tuple?
(define mk-tuple (lambda (x y) (list x y)))


; mk-rec-tuple:= [symbol?  denotable? (list-of tuple?)] => tuple?
 (define mk-rec-tuple (lambda (x y rec-env) 
                          (mk-tuple x
                             (let ((y1 (denotable->expressible y)))
                                (if (functionHandle? y1)
                                    (cases functionHandle y1
                                      [closure (formals body env) 
                                           (expressible->denotable (rec-closure formals body env rec-env))]
                                      (else "unknown closure type"))
                                     y)))))

;;;;; mk-rec-tuple:= recurcive tuples with vector ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mk-rec-tuple-vec (lambda (x y) 
                       (mk-tuple x (cases functionHandle y
                                        [closure (formals body env)
                                               (let* ((temp-vec (make-vector 2 x)) (env-vec (vector-set! temp-vec 1 env))) 
                                               (rec-closure formals body temp-vec))]
                                        (else  y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Env Env?
        [empty-env]
        [extended-env (tuples (list-of tuple?)) (outer-env Env?)]
        )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;lookupEnv : [symbol? Env?] -> expressible?
;; looks for id represented by var in given environment and returns the denoted value  if found
;; throws error if id is not found or env is not good type
(define lookupEnv(
                   lambda (var env)
                          (cases Env env
                           [empty-env () (error "lookupEnv" "unbound identifier:" var)]    
                           [extended-env (tuples outer-env)
                                     (let ((tpl (findf (lambda(u)(equal? var (first u))) tuples)))
                                          (if (not tpl)
                                               (lookupEnv var outer-env)
                                               (second tpl)))] ;wait, we now have values in store
                                                 
                            (else (error "lookupEnv" "bad environment ~a" env))
                     
                     )))


                            
;(lookupEnv 'x)
;(lookupEnv 'foo)

;; mk-tuple:= [(list-of symbol?) (list-of denotable?)] => (list-of tuple?)
(define mk-tuples (lambda(ids vals) 
                   (if (not (equal? (length ids) (length vals)))
                            (error "mk-tuples: " "mismatch in environment tuples")
                       (map mk-tuple ids vals))))                     
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define a1 (extended-env (list (mk-tuple 'x (expressible->denotable 2))
                               (mk-tuple 'y (expressible->denotable 3))
                               (mk-tuple 'z (expressible->denotable 4))
                               (mk-tuple 'b (expressible->denotable 0))) 
                               (empty-env)))
(define a2 (extended-env (list (mk-tuple 'a (expressible->denotable 20))
                               (mk-tuple 'b (expressible->denotable 30))
                               (mk-tuple 'c (expressible->denotable 40))) a1))
(define a3 (extended-env (list (mk-tuple 's (expressible->denotable 200))
                               (mk-tuple 'u (expressible->denotable 300))
                               (mk-tuple 'v (expressible->denotable 400))) a2))

(check-equal? 200 (denotable->expressible (lookupEnv 's a3)))
(check-equal? 30 (denotable->expressible (lookupEnv 'b a3)))
(check-equal? 4 (denotable->expressible (lookupEnv 'z a3)))
