#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt")

;; parse: list? --> Ast?
;; throws error if input is not in right format
(define parse
  (lambda (ls)
    (cond
      [(null? ls) (error "Invalid input (null)")]
     
      [(list? ls) ;process the list by traversing it
         (let ((head (first ls))(tail (rest ls)))
          (cond
                [(number? head) (num head)]
	    
                [(boolean? head)(bool head)]

                [(op? head)  (letrec ((opInfo (lookup-op head))   ;find the operator metadata from the operator table
                                        (arity (second opInfo)))   ;get the arity
                        
                                  (if (or (equal? arity 'n) (and (number? arity) (equal? arity (length tail)))) 
                                      (primApp head (map parse tail))
                                      (error "Number of arguments do not match with arity of operator")))]

                [(equal? 'assume head) (if (not (equal? 2 (length tail)))
                                           (error "parse :" "Bad syntax for assume")
                                           (letrec ((ast-binds (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  (first tail)))
                                                   (ast-body  (parse (second tail))))
                                           (assume ast-binds ast-body)))]

                [(equal? 'assume& head) (if (not (equal? 2 (length tail)))
                                           (error "parse :" "Bad syntax for assume")
                                           (letrec ((ast-binds (map 
                                                                 (lambda(u)
                                                                        (mk-bind (first u)
                                                                                 (parse (second u)))) 
                                                                  (first tail)))
                                                   (ast-body  (parse (second tail))))
                                           (assume& ast-binds ast-body)))]

                [(equal? 'ifte head) (if (not (equal? 3 (length tail))) 
                                              (error "parse :" "Bad syntax for ifte") 
                                           (letrec ((test (parse (first tail))) 
                                                    (then (parse (second tail)))
                                                    (else (parse (third tail))))
                                                               (ifte test then else)))]

                [(and (symbol? head) (zero? (length tail)))        (id head)]
                
                [else           (error "parse :" "bad type")]))]
       (else (parse (list ls))) ;single item can be converted into list to enter the main code
      
  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primApp and id