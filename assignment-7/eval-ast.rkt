#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt"  "env.rkt" "parser.rkt" )

(define applyk (lambda (k v) (k v)))

(define map/k (lambda(f ls k)
                (if (null? ls) (k '())
                    (f (car ls) (lambda(v)
                                  (map/k f (cdr ls) (lambda (w)(k (cons v w)))))))))

(define call-by-name/k (lambda (rands env k)
                         (map/k
                           (lambda (x k)
                             (eval-ast/k x env  (lambda(v) (k (expressible->denotable v)))))
                           rands
                           k)))

;(define call-by-name/k (lambda (rands env k)
;                         (map/k
;                           (lambda (x k)
;                             (k (expressible->denotable x)))
;                            (let ((val (expressible->denotable x)))
;                                (k val)))
;                           rands
;                           k)))


(define apply-closure/k (lambda (c rands envOuter k)
                          (cases functionHandle c
                            [rec-closure (formals body env r)
                                         (call-by-name/k rands envOuter (lambda(args)
                                                                           (map/k (lambda (x k)
                                                                                    (k (mk-rec-tuple (first x) (second x) r)))
                                                                                  r
                                                                                  (lambda(rr)
                                                                                    (eval-ast/k body
                                                                                                (extended-env rr  (extended-env (mk-tuples formals args) env))
                                                                                                k)))))]

                            [closure (formals body env)
                                     (call-by-name/k rands envOuter (lambda (args)
                                                                       (eval-ast/k body  (extended-env (mk-tuples formals args) env) k)))]

                            (else (error 'not_a_function_handle)))))

(define eval-ast/k
  (lambda (ast env k)
    (cases Ast ast

      [num (n)  (applyk k n)]

      [bool(b) (applyk k b)]

      [function (formals body)    (applyk k (closure formals body env))]

      [ifte (test then else) (eval-ast/k test env
                                         (lambda (b)
                                           (if (boolean? b)
                                               (if b
                                                   (eval-ast/k then env k)
                                                   (eval-ast/k else env k))
                                               (else (error 'eval-ast/k "ifte:test condition must be boolean instead of ~a" b)))))]

      [setRef (var val) (eval-ast/k val
                                    env
                                    (lambda (v)
                                      (lookupEnv/k  (cases Ast var  [id (s) s] (else (error "bad id type")))
                                                    env
                                                    (lambda (l)
                                                      (*setRef/k l  v k)))))]

      [id (s) (lookupEnv/k s env
                           (lambda(v)
                             (k  (denotable->expressible v))))]

      [primApp (s rands) (letrec ([proc   (op s)])
                           (map/k (lambda(u k)(eval-ast/k u env k))
                                  rands
                                  (lambda(args) (k (apply proc args)))))]

      [applyf (fid rands) (eval-ast/k fid env (lambda(c) (apply-closure/k c rands env k)))]

      [assume (binds body)
              (map/k  (lambda(u k)
                        (eval-ast/k  (second u)
                                     env
                                     (lambda(v)
                                       (k (mk-tuple (first u) (expressible->denotable  v))))))
                      binds
                      (lambda (tpls)
                        (eval-ast/k body
                                    (extended-env tpls env)
                                    k)))]

      [assume* (binds body)
               (map/k  (lambda(u k) (eval-ast/k (second u)
                                                env
                                                (lambda(v) (k (mk-tuple (first u)  (expressible->denotable v))))))
                       binds
                       (lambda (tplsAll)
                         (letrec (;; all tuples which have closures build recursive environment
                                  [closure-tuples (filter (lambda(u)
                                                            (functionHandle?
                                                             (denotable->expressible (second u)))) tplsAll)])

                           ;; make all tuples with closures recursive
                           (map/k  (lambda(u k) (k (mk-rec-tuple (first u) (second u) closure-tuples)))
                                   tplsAll
                                   (lambda (recTplsAll)
                                     ;evaluate the body in the extended environment
                                     (eval-ast/k body
                                                 (extended-env recTplsAll env)
                                                 k))))))]

      [seq (exps) (map/k (lambda(u k)(eval-ast/k u env k))
                         exps
                         (lambda(args)
                           (k (list-ref args (- (length args) 1)))))]




      ) ;end of cases
    ); end of lambda
  ); end of eval-ast/k
