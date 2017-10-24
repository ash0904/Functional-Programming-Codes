
#lang racket
(provide (all-defined-out))
(require eopl)
(require racket/trace)
(require rackunit)
(require rackunit/text-ui)
(require "ast.rkt" "op.rkt")

  
;; parse: list? --> Ast?
;; throws error if input is not in right format
(define parse
  (lambda (ls)
    (cond
      [(null? ls) (error "Invalid input (null)")]
   ;;constructs with more than one fields  
      [(list? ls) 
         (let ((head (first ls))(tail (rest ls)))
          (cond
               [(IsOp? head)  (letrec ((opInfo (lookupOp head))   ;find the operator metadata from the operator table
                                        (arity (second opInfo)))   ;get the arity
                                   

                                  (if (or (equal? arity 'n) (and (number? arity) (equal? arity (length tail)))) 
                                      (primApp head (map parse tail))
                                      (error "Number of arguments do not match with arity of operator")))]

              
                  [(equal? 'setRef head) (if (equal? (length tail) 2) 
                                         (letrec ((var-exp (car tail)) (val-exp (cadr tail)))
                                             (setRef (parse var-exp) (parse val-exp)))
                                         (error "parse :" "bad syntax for setRef"))]
                  
                   [(equal? 'seq head)   (letrec ((a (map parse tail)))
                                                  (seq a))]
                                       

                [(equal? 'assume head) (if (not (equal? 2 (length tail))) 
                                           (error "parse :" "Bad syntax for assume")
                                           (letrec ( (concrete-binds (first tail))
                                                     (ast-binds  (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  concrete-binds))
                                                      (concrete-body (second tail))
                                                      (ast-body (parse concrete-body)))
                                           (assume ast-binds ast-body)))]

                 [(equal? 'assume* head) (if (not (equal? 2 (length tail))) 
                                           (error "parse :" "Bad syntax for assume*")
                                           (letrec ( (concrete-binds (first tail))
                                                     (ast-binds  (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  concrete-binds))
                                                      (concrete-body (second tail))
                                                      (ast-body (parse concrete-body)))
                                           (assume* ast-binds ast-body)))]

                 [(equal? 'assume& head) (if (not (equal? 2 (length tail))) 
                                           (error "parse :" "Bad syntax for assume*")
                                           (letrec ( (concrete-binds (first tail))
                                                     (ast-binds  (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  concrete-binds))
                                                      (concrete-body (second tail))
                                                      (ast-body (parse concrete-body)))
                                           (assume& ast-binds ast-body)))]

                 [(equal? 'assume-v head) (if (not (equal? 2 (length tail))) 
                                           (error "parse :" "Bad syntax for assume*")
                                           (letrec ( (concrete-binds (first tail))
                                                     (ast-binds  (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  concrete-binds))
                                                      (concrete-body (second tail))
                                                      (ast-body (parse concrete-body)))
                                           (assume-v ast-binds ast-body)))]

 
	         [(equal? 'function head) (if (not (equal? 2 (length tail))) 
                                              (error "parse :" "Bad syntax for function")
                                              (letrec ((formals (first tail))  (body (second tail)))
                                                   (function formals (parse body))))]

                 ;; making concrete syntax seemless for primitive operators' application and user defined function application
                 ;; applyf
                 [(id? head)        (applyf (parse head) (map parse tail))]

                 [(equal? 'ifte head) (if (not (equal? 3 (length tail))) 
                                              (error "parse :" "Bad syntax for ifte") 
                                           (letrec ((test (parse (first tail))) 
                                                    (then (parse (second tail)))
                                                    (else (parse (third tail))))
                                      (ifte test then else)))]

                 (else              (error "parse :" "bad type"))))]
      ;;constructs with only one field without any descriptor tag
       (else  (cond
                [(number? ls)    (num ls)]
                [(boolean? ls)  (bool ls)]
                [(id? ls)         (id ls)]
                (else (error "bad input to parser"))))
      
  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-equal? (parse '3)
              (num 3)
              "parse:test1")
(check-equal? (parse 'x)
              (id 'x)
              "parse:test2")
(check-equal? (primApp '+ (list (num 2) (num 3)))
              (parse '(+ 2 3))
              "parse:test3")
(check-equal? (primApp '+ (list (id 'x) (id 'y)))
              (parse '(+ x y))
              "parse:test4")
(check-equal? (parse '(+ 3 x))
              (primApp '+ (list (num 3) (id 'x)))
              "parse:test5")
(check-equal? (parse '(- 4 (+ 3 x)))
              (primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x)))))
              "parse:test6") 
(check-equal? (parse '(* 5 (- 4 (+ 3 x))))
              (primApp '* (list (num 5) (primApp '- (list (num 4) (primApp '+ (list (num 3) (id 'x)))))))
              "parse:test7")
(check-equal? (parse '(assume ((x 2)(y 3)) (+ x y)))
              (assume (list (list 'x (num 2))(list 'y (num 3))) (primApp '+ (list (id 'x) (id 'y))))
              "parse:test8")
(check-equal? (parse '(+  1 2 3))
              (primApp '+ (list (num 1) (num 2) (num 3)))
              "parse:test9")
(check-equal? (parse '(+ 3 x 5 6)) 
              (primApp '+ (list (num 3) (id 'x) (num 5) (num 6)))
              "parse:test10")
(check-equal? (parse '(+ (- x 2)(* y 3)))
              (primApp '+ (list (primApp '- (list (id 'x) (num 2))) (primApp '* (list (id 'y) (num 3)))))
              "parse:test11")
(check-equal? (parse '(foo 3 4 x))
              (applyf (id 'foo) (list (num 3) (num 4) (id 'x))) "parse:test12")

(check-equal? (parse '(assume ((foo (function (x y) (+ x y)))) (foo 2 3)))
              (assume (list (list 'foo (function '(x y) (primApp '+ (list (id 'x) (id 'y)))))) (applyf (id 'foo) (list (num 2) (num 3))))
              "parse:test13")

(check-equal? (parse '(assume ((x 20) (y (function (a) (ifte (IsZero? (- a 1)) (+ a 10) a)))) (y (+ x 2))))
                     (assume (list (list 'x (num 20)) (list 'y (function '(a) (ifte (primApp 'IsZero? (list (primApp '- (list (id 'a) (num 1))))) (primApp '+ (list (id 'a) (num 10))) (id 'a)))))
                                         (applyf (id 'y) (list (primApp '+ (list (id 'x) (num 2))))))
              "parse:ifte:test14")

(check-equal? (parse '(assume* ((Even? (function(x)(ifte (IsZero? x) #t
                                                   (Odd? (- x 1)))))
                                         (Odd? (function(x)(ifte (IsZero? x) #f
                                                   (Even? (- x 1))))))
                                (Odd? 20)))
               (assume* (list
                          (list 'Even? (function '(x) (ifte (primApp 'IsZero? (list (id 'x))) (bool #t) (applyf (id 'Odd?) (list (primApp '- (list (id 'x) (num 1))))))))
                          (list 'Odd? (function '(x) (ifte (primApp 'IsZero? (list (id 'x))) (bool #f) (applyf (id 'Even?) (list (primApp '- (list (id 'x) (num 1)))))))))
                 (applyf (id 'Odd?) (list (num 20)))) "parse:recursion:test15")

(check-equal? (setRef (primApp '+ (list (id 'x) (id 'y))) 
                      (primApp '- (list (id 'a) (id 'b))))
              (parse '(setRef (+ x y) (- a b))))
