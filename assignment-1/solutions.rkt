#lang racket
(require eopl)
(provide (all-defined-out))

;;q1
(define wrap (lambda(ls)
             (cond
               [(null? ls) '()]
               (else (cons (cons (car ls) '()) (wrap (cdr ls))))
               )))

;;q2
(define count-occurrences (lambda(x ls)
                            (cond
                              [(null? ls) 0]
                              [(list? (car ls)) (+ (count-occurrences x (car ls)) (count-occurrences x (cdr ls)))]
                              [(eq? x (car ls)) (+ 1 (count-occurrences x (cdr ls)))]
                              (else (count-occurrences x (cdr ls)))
                              )))

;;q3
(define merge (lambda(ls1 ls2)
               (cond
                 [(null? ls1) ls2]
                 [(null? ls2) ls1]
                 [(< (first ls1) (first ls2)) (cons (first ls1) (merge (cdr ls1) ls2))]
                 (else (cons (first ls2) (merge ls1 (cdr ls2))))
                 )))

;;q4
(define prod (lambda(x ls)
              (cond
              [(null? ls) '()]
              (else (cons (list x (car ls)) (prod x (cdr ls))))
              )))

(define product (lambda(ls1 ls2)
                (cond
                 [(null? ls1) '()]
                 [(null? ls2) '()]
                 (else (append ( prod (car ls1) ls2) (product (cdr ls1) ls2) ))
                 )))

;;q5
(define traverse (lambda(root)
                  (cond
                  [(null? root) '()]
                  [(and (null? (second root)) (null? (third root))) (cons (car root) '())]
                  [(null? (third  root)) (append (traverse (second root)) (cons (car root) '()))]
                  [(null? (second  root)) (append (cons (car root) '())  (traverse (third root)))]
                  (else (append (traverse (second root)) (append (cons (car root) '())  (traverse (third root)))))
                   )))

;;definitions for q6 and q9
(define-datatype tree tree?
  [null]		;;; Null
  [node (val number?)	;;; Value of the node
        (left tree?)	;;; Left subtree
        (right tree?)])	;;; Right subtree

(define node-val (lambda(t)
                     (cases tree t
                     [node (val left right) val]
                     (else 'wrong-type))))

(define left-subtree (lambda(t)
                     (cases tree t
                       [node (val left right) left]
                       (else 'wrong-type))))

(define right-subtree (lambda(t)
                     (cases tree t
                       [node (val left right) right]
                       (else 'wrong-type))))
(define chk-tree? (lambda(t)
                (cases tree t
                  [node (val left right) #f]
                  (else #t))))

;;q6
(define findpath (lambda(val root)
                 (cond
                  [(chk-tree? root) '()]
                  [(eq? val (node-val root)) '()]
                  [(< val (node-val root)) (cons 'left (findpath val (left-subtree root)))]
                  (else (cons 'right (findpath val (right-subtree root))))
                  )))

;;q7
(define ncurry (lambda(f n args)
                (cond
                  [(zero? n) (apply f (reverse args))]
                (else
                 (lambda (x)
                   (ncurry f (- n 1) (cons x args))))
                )))

(define curry (lambda(f n)
               (ncurry f n '())))

;;q8
(define is-subseq (lambda(ls1 ls2)
                  (cond
                   [(null? ls1) #true]
                   [(null? ls2) #false]
                   [(eq? (car ls1) (car ls2)) (is-subseq (cdr ls1) (cdr ls2))]
                   (else (is-subseq ls1 (cdr ls2)))
                   )))

;;q9
(define tree-reduce (lambda(x f tree)
                     (cond
                      [(chk-tree? tree) x]
                      (else (f (node-val tree) (tree-reduce x f (left-subtree tree)) (tree-reduce x f (right-subtree tree))))
                      )))

;; ------------over-----------
