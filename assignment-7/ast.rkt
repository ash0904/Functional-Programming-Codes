
#lang racket
(require eopl/eopl)
(provide (all-defined-out))

;;Ast : num  | bool | primApp | id | assume | ifte | function | applyf | assume*

;;;; op.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;; store.rkt
(define store '())

(define init-store (lambda()(set! store '())))

;; newRef : storable? -> reference?
;; returns a new reference with stored value v
(define *newRef (lambda(v)(begin 
                           (set! store (append store (list v))) 
                           (- (length store) 1))))

     

;; deRef : denotable? -> storable?
;; returns the stored value for a given reference

(define *deRef (lambda(l) (let ((len (length store)))
                           (if (or (zero? len) (> l len)) 
                               (error "store- deRef: out of bound")
                               (list-ref store l)))))

;; setRef : denotable? storable? -> void
;; changes the value stored at location referenced by l to v
;; iterates through the store to point to correct index
;; creates a new store with updated value at location l

(define *setRef/k
         (lambda (l v k)
                 (letrec ((setRef-inner/k 
                             (lambda (ls n k)
                               (if (zero? n) 
                                   (k (cons v (cdr ls)))
                                   (if (null? ls)
                                       (k (error (list "*setRef/k : out of bound" n)))
                                       (setRef-inner/k 
                                                   (cdr ls) 
                                                   (- n 1)
                                                   (lambda (v) (k (cons (car ls) v)))))))))
                          (setRef-inner/k store l (lambda(w) (k (set! store w)))))))

;;;; ast.rkt
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
