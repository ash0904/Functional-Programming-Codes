#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt"  "env.rkt" "parser.rkt" "eval-ast.rkt" )
;(require rackunit)

 (define G (empty-env))
    (init-store)
    (check-equal? 210 (eval-ast/k
	(parse '(assume* ((counter 20) (sum 0))
		                (assume* ((print-sum (function ()
		                                               (ifte (IsZero? counter)
		                                                  sum
		                                                  (seq 
		                                                     (setRef sum (+ sum counter))
		                                                     (setRef counter (- counter 1))
		                                                     (print-sum))))))
		                         (print-sum))))  G (lambda(v) v)))

    (check-equal? 25 (length store))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 210 (eval-ast/k (parse '(assume* ((counter 20) (sum 0))
		                (assume* ((print-sum (function (counter sum)
		                                               (ifte (IsZero? counter)
		                                                     sum
		                                                  ( print-sum (- counter 1)
		                                                              (+ sum counter))))))
		                            (print-sum counter sum))
		                     )) 
		                                         G (lambda(v) v)))

    (check-equal? 67 (length store))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; recursion - factorial with one operand 
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 720 (eval-ast/k (parse '(assume* ((n 6) (a 1))
		                          (assume* ((fac (function (n) 
		                                           (ifte (IsZero? n)
		                                             1
		                                             (*  n (fac (- n 1)))))))
		                                   (fac n))))
		              (empty-env) (lambda(v) v)))

    (check-equal? (length store) 18)

    ;; recursion - factorial with no operand
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 720 (eval-ast/k (parse '(assume* ((n 6) (a 1))
		                            (assume* ((fac (function ()
		                                             (ifte (IsZero? n) 
		                                                a
		                                                (seq 
		                                                    (setRef a (* a n))
		                                                    (setRef n (- n 1))
		                                                    (fac))))))
		                                        (fac)))) (empty-env) (lambda(v) v)))

    (check-equal? (length store) 11) 

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 10 (eval-ast/k (parse '(assume ((foo (function (x y) (ifte (IsZero? x) 5 (/ 20 x)))))
		        (foo 2 (/ 5 0) ))) (empty-env) (lambda(v) v)))

    (check-equal? #t (procedure? (list-ref store 1)) "test:call-by-name")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (eval-ast/k (parse '(assume ((foo (function (x y) (seq  
		                                         x
		                                         (setRef n 5)
		                                         x
		                                         ))))
		                                 (foo (* 2 n) n) ))
		   (extended-env (list (mk-tuple 'n (expressible->denotable 1)))(empty-env)) (lambda(v) v))

    (check-equal? 5 (list-ref store 0) "first") ;;n
    (check-equal? #t (closure? (list-ref store 1)) "second") ;;foo
    (check-equal? #t (procedure? (list-ref store 2)) "third") ;;thunk
