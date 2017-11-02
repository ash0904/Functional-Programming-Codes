#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt" )

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;defining closure/rec-closure/functionHandle

(define-datatype functionHandle functionHandle?
  [closure (formals (list-of symbol?)) (body Ast?) (env Env?)]
  [rec-closure (formals (list-of symbol?)) (body Ast?) (env Env?) (rec-tuples (list-of tuple?))])

(define closure? (lambda(x)
  (if (functionHandle? x)
    (cases functionHandle x
      [closure (formals body env) #t]
      (else #f))
   #f)))

(define rec-closure? (lambda(x)
  (if (functionHandle? x)
    (cases functionHandle x
      [rec-closure (formals body env tpls) #t]
      (else #f))
   #f)))

(define expressible? (or/c number? boolean? functionHandle?))
(define ans? expressible?)
(define reference? integer?)
(define denotable? reference?)
(define storable? expressible?)
(define denotable->expressible (lambda(thing) (*deRef thing)))
(define expressible->denotable (lambda(thing) (*newRef thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defining environment tuple
; tuple?:= [symbol? denotable?] => tuple?
(define tuple? (lambda(x)
                    (and (list? x)
                         (symbol? (first x))
                         (denotable? (second x)))))

; mk-tuple:= [symbol? denotable?] => tuple?
(define mk-tuple (lambda (x y) (list x y)))


; mk-rec-tuple:= [symbol?  denotable? (list-of tuple?)] => tuple?
 (define mk-rec-tuple (lambda (x y rec-env)
                          (mk-tuple x
                             (let ((y1 (denotable->expressible y)))
                                (if (functionHandle? y1)
                                    (cases functionHandle y1
                                      [closure (formals body env)
                                           (expressible->denotable (rec-closure formals body env rec-env))]
                                      (else "unknown closure type"))
                                     y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype Env Env?
        [empty-env]
        [extended-env (tuples (list-of tuple?)) (outer-env Env?)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lookupEnv/k(
                   lambda (var env k)
                          (cases Env env
                           [empty-env () (k (error "lookupEnv/k" "unbound identifier:" var))]
                           [extended-env (tuples outer-env)
                                     (let ((tpl (findf (lambda(u)(equal? var (first u))) tuples)))
                                          (if (not tpl)
                                               (lookupEnv/k var outer-env k)
                                               (k (second tpl))))] ;wait, we now have values in store

                            (else (k (error "lookupEnv/k" "bad environment ~a" env)))

                     )))

;; mk-tuple:= [(list-of symbol?) (list-of denotable?)] => (list-of tuple?)
(define mk-tuples (lambda(ids vals)
                   (if (not (equal? (length ids) (length vals)))
                            (error "mk-tuples: " "mismatch in environment tuples")
                       (map mk-tuple ids vals))))
