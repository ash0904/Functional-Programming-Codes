
#lang racket
(provide (all-defined-out))
(require eopl)

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

