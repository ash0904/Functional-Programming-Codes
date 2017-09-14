#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt" "env.rkt" "parser.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;eval-ast : [Ast? env?]--> ans? or error
;; throws error for arity mismatch and unbound identifier

(define reduce (lambda (f tenv ls)
                     (if (null? ls) tenv (reduce f (f tenv (car ls)) (cdr ls)))
                     ))

(define eval-ast
  (lambda (ast env)
    (cases Ast ast 

      [num (n)  n]
 
      [bool(b) b]

      [id (s)  (denotable->expressible(lookup-env s env))]
 
      [ifte (test then else) (let ((b (eval-ast test env)))
                                  (cond
                                  [(boolean? b) (if b (eval-ast then env)
                                                   (eval-ast else env))]
                                  (else (error 'eval-ast "ifte:test condition must be boolean instead of ~a" b))))]

      [assume (binds body)   (let ((tpls (map  (lambda(u) 
                                                   (mk-tuple  (first u)  
                                                              (expressible->denotable 
                                                                  (eval-ast (second u) env)))) 
                                           binds))) ;evaluate asts to get values to be bound to identifiers
                                  (eval-ast body (extended-env tpls env)))] ;evaluate the body in the extended envirnment

      [assume& (binds body)   (let ((tpls (reduce (lambda(tenv u) 
                                                   (extended-env (list
                                                                  (mk-tuple  (first u)  
                                                                             (expressible->denotable 
                                                                              (eval-ast (second u) tenv)))) tenv)) 
                                          env  binds) ))
                                (eval-ast body tpls))] 
     
      [primApp (s rands) (letrec ((proc   (op s))     ;get the operator procedure
                                   (args   (map (lambda(u)(eval-ast u env)) rands)))   ;evaluate operands to get actual arguments
                           (apply proc args))]
      )))
