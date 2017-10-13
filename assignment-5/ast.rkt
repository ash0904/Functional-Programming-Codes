#lang racket
(provide (all-defined-out))
(require eopl)
(require "op.rkt")

;;Ast : num  | bool | primApp | id | assume | ifte | function | applyf | assume*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Ast Ast?
  [num (n number?)]
  [bool (b boolean?)]
  [id (s symbol?)]
  [primApp (op IsOp?) (rands (list-of Ast?))]
  [ifte (test Ast?) (then Ast?) (else Ast?)]
  [assume (binds (list-of bind?)) (exp Ast?)] 
  [function (formals (list-of id?)) (exp Ast?)]
  [applyf (fid Ast?) (rands (list-of Ast?))]
  [assume* (binds  (list-of bind?)) (exp Ast?)])

;; mkbind:= [symbol? Ast?] => bind?
(define mk-bind (lambda(s a)(list s a)))
(define bind? (lambda(b)(and (symbol? (first b))(Ast? (second b)))))

(define keywords '(function assume  ifte))
(define id? (and/c symbol? (lambda(s) (not (memq s keywords)))))