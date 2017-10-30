
#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt" "op.rkt" "env.rkt" "parser.rkt" "store.rkt")

;; apply-closure:= [functionHandle? (list-of Ast?) Env?] => expressible?

(define apply-closure (lambda (c rands envOuter)
(cases functionHandle c
   [rec-closure-v (formals body v)
             (let ((args  (map (lambda (x)(expressible->denotable (eval-ast x envOuter))) rands)))
                    
              ;(begin (display (list 'args= args 'rec-env= (extended-env (mk-tuples formals args) (vector-ref v 0))))
               (eval-ast body  (extended-env (mk-tuples formals args) (vector-ref v 0))))]
  
   [rec-closure (formals body env r)
		(let ((args  (map (lambda (x)(expressible->denotable
					      (eval-ast x envOuter))) rands))
                  (rr    (map (lambda (x) (mk-rec-tuple (first x) (second x) r)) r)))

             (eval-ast body (extended-env rr 
                                        (extended-env (mk-tuples formals args) env))))]

   [closure (formals body env)
	    (let ((args (map (lambda (x)(expressible->denotable
					 (eval-ast x envOuter))) rands)))
            (eval-ast body  (extended-env (mk-tuples formals args) env)))]

   (else (error 'not_a_function_handle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define apply-closure-vec (lambda (c rands envOuter)
(cases functionHandle c
   [rec-closure (formals body env-vec a)
            (let ((args (map (lambda (x)(eval-ast x envOuter)) rands)) (env (vector-ref env-vec 1)) (fid  (vector-ref env-vec 0)))
             (eval-ast body (extended-env (list (mk-tuple fid c)) (extended-env (mk-tuples formals args) env))))]
   [closure (formals body env)
           (let ((args (map (lambda (x)(eval-ast x envOuter)) rands)))
            (eval-ast body  (extended-env (mk-tuples formals args) env)))]
   (else (error 'not_a_function_handle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;eval-ast : [Ast? env?]--> ans? or error
;; throws error for arity mismatch and unbound identifier
(define eval-ast
  (lambda (ast env)
    (cases Ast ast 

      [num (n)  n]
      
      [bool(b) b]

      [primApp (s rands) (letrec ((proc   (op s))                                     ;get the operator procedure
                                  (args   (map (lambda(u)(eval-ast u env)) rands)))   ;evaluate operands to get actual arguments
                           (apply proc args))]

      [ifte (test then else) (let ((b (eval-ast test env)))
                                  (cond
                                  [(boolean? b) (if b (eval-ast then env)
                                                   (eval-ast else env))]
                                  (else (error 'eval-ast "ifte:test condition must be boolean instead of ~a" b))))]
      [function (formals body)    (closure formals body env)]

      [applyf (fid rands)(letrec ((c (eval-ast fid env)))
                                     (apply-closure c rands env))]  

      [id (s) (denotable->expressible(lookupEnv s env))]

      [assume (binds body)   
        (let ([tpls (map  (lambda(u)   (mk-tuple  
                                              (first u)  
                                              (expressible->denotable 
                                                   (eval-ast (second u) env))))
                      binds)]) ;evaluate asts to get values to be bound to identifiers
              
              ;evaluate the body in the extended environment
              (eval-ast body (extended-env tpls env)))]

       [assume-v (binds body)
          (letrec (
                   ;; all evaluated tuples
                   [tplsAll (map  (lambda(u)  (mk-tuple
                                                   (first u)
                                                    (expressible->denotable
                                                        (eval-ast (second u) env))))
                           binds)]

                   ;; all tuples with closures as values build recursive environment
                   [closure-tuples (filter (lambda(u) (functionHandle?
                                                    (denotable->expressible (second u))))
                                 tplsAll)]


                    ;; make all tuples with closures vector-closures using vector 
                  [v-tuples (map  (lambda(u)
                                              (mk-vec-tuple (first u) (second u)))
                                          tplsAll)]
                    ;; fix the vector to point to vector recursive environment
                  [vtplsAll (map  (lambda(u)
                                              (vec-tuple! (first u) (second u) v-tuples))
                                          v-tuples)]
                  
                   )
                 ;evaluate the body in the extended environment
                (eval-ast body (extended-env vtplsAll env)))] 


       [assume* (binds body)   
         (letrec (
                  ;; all evaluated tuples
                  [tplsAll (map  (lambda(u)  (mk-tuple 
                                                  (first u) 
                                                    (expressible->denotable 
                                                       (eval-ast (second u) env))))
                          binds)]

                  ;; all tuples which have closures build recursive environment
                  [rec-tuples (filter (lambda(u) (functionHandle? 
                                                    (denotable->expressible (second u))))
                                tplsAll)]


                  ;; make all tuples with closures recursive
                  [recTplsAll   (map  (lambda(u)
                                       (mk-rec-tuple (first u) (second u) rec-tuples))
                                                     
                                   tplsAll)])
                         ;evaluate the body in the extended environment
                         (eval-ast body (extended-env recTplsAll env)))] 
      
       [assume& (binds body)   
         (letrec (
                  ;; all evaluated tuples
                  [tplsAll (map  (lambda(u)  (mk-tuple 
                                                  (first u) 
                                                    (expressible->denotable 
                                                       (eval-ast (second u) env))))
                          binds)]

                  ;; all tuples which have closures build recursive environment
                  [rec-tuples (filter (lambda(u) (functionHandle? 
                                                    (denotable->expressible (second u))))
                                tplsAll)]


                  ;; make all tuples with closures recursive
                  [recTplsAll   (map  (lambda(u)
                                       (mk-rec-tuple (first u) (second u) rec-tuples))
                                                     
                                   tplsAll)])
                         ;evaluate the body in the extended environment
                         (eval-ast body (extended-env recTplsAll env)))]
 
       [trace-assume (binds body)   
         (letrec (
                  ;; all evaluated tuples
                  [tplsAll (map  (lambda(u)  (mk-tuple 
                                                  (first u) 
                                                    (expressible->denotable 
                                                       (eval-ast (second u) env))))
                          binds)]

                  ;; all tuples which have closures build recursive environment
                  [rec-tuples (filter (lambda(u) (functionHandle? 
                                                    (denotable->expressible (second u))))
                                tplsAll)]


                  ;; make all tuples with closures recursive
                  [recTplsAll   (map  (lambda(u)
                                       (mk-rec-tuple (first u) (second u) rec-tuples))
                                                     
                                   tplsAll)])
                         ;evaluate the body in the extended environment
                         (eval-ast body (extended-env recTplsAll env)))]
     
      [setRef (var val)(*setRef (lookupEnv 
                                   (cases Ast var 
                                         [id (s) s]
                                         (else (error "bad id type")))
                                     env)
                               (eval-ast val env))]
      
      [seq (exps) (letrec ([args (map (lambda(u)(eval-ast u env)) exps)]
                           [n    (length args)])
                     (list-ref args (- n 1)))])))


