#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt" "op.rkt" "env.rkt" "parser.rkt")

;; apply-closure:= [functionHandle? (list-of Ast?) Env?] => expressible?
(define apply-closure (lambda 
                          (c rands envOuter)
                        (cases functionHandle c

                          [rec-closure (formals body env r)
                                       (letrec ((args  (map (lambda (x)(expressible->denotable (eval-ast x envOuter))) rands))
                                                (delta (mk-tuples formals args))
                                                (rr    (map (lambda (x) (mk-rec-tuple (first x) (second x) r)) r)))

                                         (eval-ast body (extended-env rr 
                                                                      (extended-env delta env))))]

                          [closure (formals body env)
                                   (letrec ((args (map (lambda (x)(eval-ast x envOuter)) rands))
                                            (delta (mk-tuples formals args)))
           
                                     (eval-ast body  (extended-env delta env)))]

                          (else (error 'not_a_function_handle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;eval-ast : [Ast? env?]--> ans? or error
;; throws error for arity mismatch and unbound identifier
(define eval-ast
  (lambda (ast env)
    (cases Ast ast 
      [num (n)  n]
      [bool(b) b]
      [id (s) (denotable->expressible(lookupEnv s env))]
      [primApp (s rands) (letrec ((proc   (op s))                                     ;get the operator procedure
                                  (args   (map (lambda(u)(eval-ast u env)) rands)))   ;evaluate operands to get actual arguments
                           (apply proc args))]

      [ifte (test then else) (let ((b (eval-ast test env)))
                               (cond
                                 [(boolean? b) (if b (eval-ast then env)
                                                   (eval-ast else env))]
                                 (else (error 'eval-ast "ifte:test condition must be boolean instead of ~a" b))))]

      [assume (binds body)   
              (let (
                    ;; all evaluated tuples
                    [tpls (map  (lambda(u)   (mk-tuple  (first u)  
                                                        (expressible->denotable 
                                                         (eval-ast (second u) env)))) 
                                binds)])

                (eval-ast body (extended-env tpls env)))] ;evaluate the body in the extended environment

      [assume* (binds body)   
               (letrec (
                        ;; all evaluated tuples
                        [tpls (map  (lambda(u)  (mk-tuple (first u) 
                                                          (expressible->denotable 
                                                           (eval-ast (second u) env))))
                                    binds)]

     
                        ;; make all tuples with closures recursive
                        [rec-tuples   (map  (lambda(u)
                                              (mk-rec-tuple (first u) (second u) tpls))
                                                     
                                            tpls)])

                 (eval-ast body (extended-env rec-tuples env)))] ;evaluate the body in the extended environment

   
      [function (formals body)    (closure formals body env)]

      [applyf (fid rands)(letrec ((c (eval-ast fid env)))
                           (apply-closure c rands env))]  
      )))