#lang racket
(provide (all-defined-out))
(require eopl)

(define (^ . args)
  (define power(lambda(base ls)
                 (cond
                   [(null? ls) base]
                   (else (power (expt base (car ls))  (cdr ls)))))
                 )
   (if (eq? 1 (length args)) (car args) (power (car args) (cdr args))))


(define opTable (list (list '+ + 'n)
                      (list '- - 'n)
                      (list '* * 'n)
                      (list '/ / 'n)
                      (list '^ ^ 'n)
                      (list 'IsZero? zero? 1)))

;; lookupOp: symbol? -> operator | null
;; looking for match for operator symbol in a table for supported operations
;; if match is found, metadata (p:procedure ar:arity) is returned

(define lookup-op
   (lambda (sym)
     (let ((vals (filter 
                    (lambda(u)(equal? (car u) sym)) 
                    opTable)))

             (if (not (null? vals))
                 (rest (car vals))
                 null))))


;; op?: symbol? -> boolean
;; looks up in the table for match, returns boolean
(define op?
   (lambda (sym)
     (not (null? (lookup-op sym)))))

;; get the procedure
(define op 
 (lambda (s)
   (first (lookup-op s))))

;;Ast : num | primApp | id | b | ifte | assume 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Ast Ast?
  [num (n number?)]
  [id (s symbol?)]
  [primApp (op op?) (rands (list-of Ast?))]
  [bool (b boolean?)]
  [ifte (test Ast?) (then Ast?) (else Ast?)]
  [assume  (binds (list-of bind?)) (exp Ast?)]
  [assume& (binds (list-of bind?)) (exp Ast?)])

;;mk-bind? [symbol? Ast?] => bind?
(define mk-bind (lambda(s a)(list s a)))

(define bind? (lambda(b)(and (symbol? (first b))(Ast? (second b)))))

