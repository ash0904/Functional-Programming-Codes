
#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)

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

(define *setRef (lambda (l v)
		 (letrec ((setRef-inner 
                           (lambda (ls n)
			     (if (zero? n) 
                                 (cons v (cdr ls))
                                 (if (null? ls)
                                     (error (list "*setRef : out of bound" n))
                                     (cons (car ls) 
                                           (setRef-inner 
                                                   (cdr ls) 
                                                   (- n 1))))))))
(set! store (setRef-inner store l)))))

;;test cases
(set! store '())
(check-equal? 0 (*newRef 10))
(check-equal? 1 (*newRef 20))
(check-equal? 2 (*newRef 30))
(check-equal? 3 (*newRef 40))
(check-equal? '(10 20 30 40) store )
(check-equal? 10 (*deRef 0))
(check-equal? 20 (*deRef 1))
(check-equal? 30 (*deRef 2))
(check-equal? 40 (*deRef 3))
(check-equal? '(10 20 30 40) store)
 (*setRef 0 'a)
 (*setRef 1 'b)
 (*setRef 2 'c)
 (*setRef 3 'd)
(check-equal? store '(a b c d))
(set! store '())
