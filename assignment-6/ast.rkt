
#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
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
  [assume* (binds  (list-of bind?)) (exp Ast?)]
  [assume& (binds  (list-of bind?)) (exp Ast?)]
  [assume-v (binds  (list-of bind?)) (exp Ast?)]

  [setRef (var Ast?) (val Ast?)]
  [seq (exps (list-of Ast?))])

;; mkbind:= [symbol? Ast?] => bind?
(define mk-bind (lambda(s a)(list s a)))
(define bind? (lambda(b)(and (symbol? (first b))(Ast? (second b)))))

(define keywords '(function assume  ifte))
(define id? (and/c symbol? (lambda(s) (not (memq s keywords)))))

;;test cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(num 3)
;(id 'x)
;(primApp '+ (list (num 3) (id 'x)))
;(primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x)))))
;(primApp '* (list (num 5) (primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x)))))))
(check-equal? (assume* (list (list 'x (num 2)) (list 'y (num 3))) (primApp '+ (list (id 'x) (id 'y))))
              (assume* (list (mk-bind 'x (num 2))(mk-bind 'y (num 3)))
              (primApp '+ (list (id 'x) (id 'y)))))
