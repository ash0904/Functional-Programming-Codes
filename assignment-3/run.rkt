#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt"  "env.rkt" "parser.rkt" "eval-ast.rkt")

;(require rackunit)
 ;   (define e1 (extended-env (list (mk-tuple 'x 2)(mk-tuple 'y 3)(mk-tuple 'z 4) (mk-tuple 'b 0)) (empty-env)))
  ;  (define e2 (extended-env (list (mk-tuple 'a 20)(mk-tuple 'b 30)(mk-tuple 'c 40)) e1))
   ; (define e3 (extended-env (list (mk-tuple 's 200)(mk-tuple 'u 300)(mk-tuple 'v 400)) e2))

    ;;lexical
    ;(check-equal? 122 (eval-ast (parse '(assume ((x 10))
;				          (assume ((x 20) (y 2)) 
;				             (assume ((x 30) (z 3)) 
;				                  (+ x y (* b z)))))) e3) "lexical-1")

 ;   (check-equal? -4 (eval-ast (parse '(assume ((x 2)(y 3)(z 4))
;				          (assume ((x 4)(z (* x 2)))
;				              (assume ((z (+ x z)) (y (- y x)))
;				                  (+ x (* z y))))))  
;				 (empty-env)) "lexical-2")

 ;   (check-equal? 0 (eval-ast (parse '(assume& ((x 4)(y (* x x)))
  ;                        		  (assume& ((z x) (w (+ z 2)))
   ;                       		      (- (^ y x) (^ (- 22 w) z)))))
    ;                 		 (empty-env)) "lexical-3")

    ;;ifte
   ; (check-equal? 31 (eval-ast (parse '(assume ((x 20)
;				                (y  (ifte (IsZero? (- a 1)) (+ a 10) a)))
;				               (+ x y)))
;			       (extended-env (list (mk-tuple 'a 1)) (empty-env))) "eval-ast: assume-ifte-true")

;    (check-equal? 30 (eval-ast (parse '(assume ((x 20)
;				                (y (ifte (IsZero? (- a 1)) (+ a 10) a)))
;				               (+ x y)))
;			       (extended-env (list (mk-tuple 'a 10)) (empty-env))) "eval-ast: assume-ifte-false")

    ;;ifte:error:test_not_boolean
;    (check-exn exn? (lambda () (eval-ast (parse '(assume ((x 20)
;				                (y (ifte  (- a 1) (+ a 10) a)))
;				               (+ x y)))
;			       (extended-env (list (mk-tuple 'a 10)) (empty-env)))) "eval-ast: assume-ifte-test-not-boolean")
