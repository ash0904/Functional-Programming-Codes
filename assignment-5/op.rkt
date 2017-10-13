#lang racket
(require eopl)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursively computes the power of numbers given as a list of arguments
(define pow (lambda (x n)
     (cond
       [(zero? n) 1]
       (else (* x (pow x (- n 1)))) )))

(define computePow (lambda (x ls)
     (cond
       [(null? ls) x]
       (else (computePow (pow x (car ls)) (cdr ls)))) ))

(define (power . ls) (computePow (car ls) (cdr ls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operator table keeps all supported operations with metadata
;; table has name, procedure,and arity for each supported operator
;; table can be enhanced by adding signature of arguments

(define opTable (list (list '+ + 'n)
                      (list '- - 'n)
                      (list '* * 'n)
                      (list '/ / 'n)
                      (list '^ power 'n)
                      (list '> > 2)                      
                      (list 'IsZero? zero? 1)))

;; lookupOp: symbol? -> operator | null
;; looking for match for operator symbol in a table for supported operations
;; if match is found, metadata (p:procedure ar:arity) is returned

(define lookupOp
   (lambda (sym)
     (let ((vals (filter 
                    (lambda(u)(equal? (car u) sym)) 
                    opTable)))

             (if (not (null? vals))
                 (rest (car vals))
                 null))))


;; op?: symbol? -> boolean
;; looks up in the table for match, returns boolean
(define IsOp?
   (lambda (sym)
     (not (null? (lookupOp sym)))))

;; get the procedure
(define op 
 (lambda (s)
   (first (lookupOp s))))