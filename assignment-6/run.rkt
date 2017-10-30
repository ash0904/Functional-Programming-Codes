#lang racket
(require eopl/eopl)
(provide (all-defined-out))
(require "eval-ast.rkt" "env.rkt" "store.rkt" "parser.rkt")
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)

 (define G (empty-env))
    (init-store)
    (check-equal? 210 (eval-ast
    	    (parse '(assume* ((counter 20) (sum 0))
                        (assume* ((print-sum (function ()
                                                       (ifte (IsZero? counter)
                                                          sum
                                                          (seq 
                                                             (setRef sum (+ sum counter))
                                                             (setRef counter (- counter 1))
                                                             (print-sum))))))
                                 (print-sum))))  G))

    (check-equal? 25 (length store))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 210 (eval-ast
    	    (parse '(assume-v ((counter 20) (sum 0))
                        (assume-v ((print-sum (function ()
                                                       (ifte (IsZero? counter)
                                                          sum
                                                          (seq 
                                                             (setRef sum (+ sum counter))
                                                             (setRef counter (- counter 1))
                                                             (print-sum))))))
                                 (print-sum))))  G))

    (check-equal? 4 (length store))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 210 (eval-ast (parse '(assume* ((counter 20) (sum 0))
                        (assume* ((print-sum (function (counter sum)
                                                       (ifte (IsZero? counter)
                                                             sum
                                                          ( print-sum (- counter 1)
                                                                      (+ sum counter))))))
                                    (print-sum counter sum))
                             )) 
                                                 G))

    (check-equal? 67 (length store))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 210 (eval-ast (parse '(assume* ((counter 20) (sum 0))
                        (assume-v ((print-sum (function (counter sum)
                                                       (ifte (IsZero? counter)
                                                             sum
                                                          ( print-sum (- counter 1)
                                                                      (+ sum counter))))))
                                    (print-sum counter sum))
                             )) 
                                                 G))

    (check-equal? 46 (length store))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; recursion - factorial with one operand 
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 720 (eval-ast (parse '(assume* ((n 6) (a 1))
                                  (assume* ((fac (function (n) 
                                                   (ifte (IsZero? n)
                                                     1
                                                     (*  n (fac (- n 1)))))))
                                           (fac n))))
                      (empty-env)))

    (check-equal? (length store) 18) 

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; recursion - factorial with one operand (assume-v)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 720 (eval-ast (parse '(assume-v ((n 6) (a 1))
                                  (assume-v ((fac (function (n) 
                                                   (ifte (IsZero? n)
                                                     1
                                                     (*  n (fac (- n 1)))))))
                                           (fac n))))
                      (empty-env)))

    (check-equal? (length store) 11)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; recursion - factorial with no operand
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 720 (eval-ast (parse '(assume* ((n 6) (a 1))
                                    (assume* ((fac (function ()
                                                     (ifte (IsZero? n) 
                                                        a
                                                        (seq 
                                                            (setRef a (* a n))
                                                            (setRef n (- n 1))
                                                            (fac))))))
                                                (fac)))) G))

    (check-equal? (length store) 11)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; recursion - factorial with no operand (assume-v)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init-store)
    (check-equal? 720 (eval-ast (parse '(assume-v ((n 6) (a 1))
                                    (assume-v ((fac (function ()
                                                     (ifte (IsZero? n) 
                                                        a
                                                        (seq 
                                                            (setRef a (* a n))
                                                            (setRef n (- n 1))
                                                            (fac))))))
                                                (fac)))) G))

    (check-equal? (length store) 4)