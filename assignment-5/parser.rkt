#lang racket
(provide (all-defined-out))
(require eopl)
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