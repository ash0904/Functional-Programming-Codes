#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt" "op.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;defining closure/rec-closure/functionHandle 
(define-datatype functionHandle functionHandle?
  [closure (formals (list-of symbol?)) (body Ast?) (env Env?)]
  [rec-closure (formals (list-of symbol?)) (body Ast?) (env Env?) (rec-tuples (list-of tuple?))])
 
(define expressible? (or/c number? boolean? functionHandle?)) 
(define ans? expressible?)
(define denotable? expressible?)
(define denotable->expressible (lambda(thing) thing))
(define expressible->denotable (lambda(thing) thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defining environment tuple
(define tuple? (lambda(x)
                    (and (list? x) 
                         (symbol? (first x)) 
                         (denotable? (second x)))))

; mk-tuple:= [symbol? denotable?] => tuple?
(define mk-tuple (lambda (x y) (list x y)))


; mk-rec-tuple:= [symbol?  denotable? Env?] => tuple?
(define mk-rec-tuple (lambda (x y rec-env) 
                          (mk-tuple x
                              (if (functionHandle? y)
                                   (cases functionHandle y
                                        [closure (formals body env) 
                                            (rec-closure formals body env rec-env)]
                                        (else "unknown closure type"))
                                 y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Env Env?
        [empty-env]
        [extended-env (tuples (list-of tuple?)) (outer-env Env?)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;lookupEnv : [symbol? Env?] -> denotable?
;; looks for id represented by var in given environment and returns its denoted value if found
;; if id is not found or env is not good type, throws error

(define lookupEnv (lambda (var env)
                          (cases Env env
                           [empty-env () (error "lookupEnv:empty-env" "unbound identifier")]
                           [extended-env (tuples outer-env)
                                            (let ((tpl (findf (lambda(u)(equal? var (first u))) tuples)))
                                                 (if (not tpl) 
                                                     (lookupEnv var outer-env)
                                                     (second tpl)))]
                          )))
                            
;(lookupEnv 'x)
;(lookupEnv 'foo)

;; mk-tuple:= [(list-of symbol?) (list-of denotable?)] => (list-of tuple?)
(define mk-tuples (lambda(ids vals) 
                   (if (not (equal? (length ids) (length vals)))
                            (error "mk-tuples: " "mismatch in environment tuples")
                       (map mk-tuple ids vals))))                     