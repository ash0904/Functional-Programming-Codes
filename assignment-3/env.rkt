#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt")

(define expressible? (or/c number? boolean?)) 
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

;;mk-tuple: [symbol? denotable?] => tuple?
(define mk-tuple (lambda (x y) (list x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Env Env?
        [empty-env]
        [extended-env (tuples (list-of tuple?)) (outer-env Env? )]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;lookup-env : (symbol? Env?) -> denotable?
;; looks for id represented by var in given environment and returns its denoted value if found
;; if id is not found or env is not good type, throws error

(define lookup-env (lambda (var env)
                          (cases Env env
                           [empty-env () (error "lookup-env:empty-env" "unbound identifier")]
                           [extended-env (tuples outer-env)
                                            (let ((tpl (findf (lambda(u)(equal? var (first u))) tuples)))
                                                 (if (not tpl) 
                                                     (lookup-env var outer-env)
                                                     (second tpl)))])))
                            

