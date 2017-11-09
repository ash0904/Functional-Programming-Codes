
#lang racket
(require eopl/eopl)
(provide (all-defined-out))
(require "ast.rkt" "env.rkt" "parser.rkt")

(define applyk (lambda (k v) (k v)))

;(define map (lambda(f ls)
;              (if (null? ls) '()
;                  (cons (f (car ls)) (map f (cdr ls))))))

(define map/k (lambda(f ls k)
              (if (null? ls) (k '())
                  (f (car ls) (lambda(v)
                                (map/k f (cdr ls) (lambda (w)(k (cons v w)))))))))

;(define call-by-value (lambda (rands env)
;                             (map 
;                                 (lambda (x) 
;                                   (expressible->denotable 
;                                         (eval-ast x env)))
;                                 rands)))
(define call-by-value/k (lambda (rands env k)
                          (map/k 
                                 (lambda (x k)
                                    (k (expressible->denotable (lambda(g) (eval-ast/k x env g)))) 
                                         ;(lambda() (eval-ast/k x env  (lambda(v) (k (expressible->denotable v)))))
                                   )
                                 rands
				 k)))

;; apply-closure:= [functionHandle? (list-of Ast?) Env? procedure?] => expressible?
;(define apply-closure (lambda (c rands envOuter)
;(cases functionHandle c
;   [rec-closure (formals body env r)
;            (let ((args  (call-by-value rands envOuter))
;                  (rr    (map (lambda (x) (mk-rec-tuple (first x) (second x) r)) r)))

;             (eval-ast body (extended-env rr 
;                                        (extended-env (mk-tuples formals args) env))))]

;   [closure (formals body env)
;           (let ((args (call-by-name rands envOuter)))
;            (eval-ast body  (extended-env (mk-tuples formals args) env)))]

;   (else (error 'not_a_function_handle)))))

(define apply-closure/k (lambda (c rands envOuter k)
(cases functionHandle c
  
   [rec-closure (formals body env r)
		  (call-by-value/k rands envOuter (lambda(args)
						    (map/k (lambda (x k)
							     (k (mk-rec-tuple (first x) (second x) r)))
							   r
							   (lambda(rr)
								 (eval-ast/k body
									   (extended-env rr  (extended-env (mk-tuples formals args) env))
									   k)))))]

   [closure (formals body env)
	    (call-by-value/k rands envOuter (lambda (args)
					         (eval-ast/k body  (extended-env (mk-tuples formals args) env) k)))]

   (else (error 'not_a_function_handle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;eval-ast/k : [Ast? env? procedure?]--> ans? or error
;; throws error for arity mismatch and unbound identifier
(define eval-ast/k
  (lambda (ast env k)
    (cases Ast ast 

      [num (n)  (applyk k n)]
      
      [bool(b) (applyk k b)]
     
      [function (formals body)    (applyk k (closure formals body env))]

     ;[ifte (test then else) (let ((b (eval-ast test env)))
     ;                             (cond
     ;                             [(boolean? b) (if b (eval-ast then env)
     ;                                                 (eval-ast else env))]
     ;                             (else (error 'eval-ast "ifte:test condition must be boolean instead of ~a" b))))]
     [ifte (test then else) (eval-ast/k test env
				       (lambda (b)
                                         (if (boolean? b)
					    (if b
					       (eval-ast/k then env k)
                                               (eval-ast/k else env k))
                                         (else (error 'eval-ast/k "ifte:test condition must be boolean instead of ~a" b)))))]

     ;[setRef (var val)(*setRef (lookupEnv 
     ;                              (cases Ast var 
     ;                                    [id (s) s]
     ;                                    (else (error "bad id type")))
     ;                                env)
     ;                          (eval-ast val env))]

     [setRef (var val) (eval-ast/k val 
                                   env 
                                   (lambda (v)  
                                         (lookupEnv/k  (cases Ast var  [id (s) s] (else (error "bad id type")))
					                env 
                                                        (lambda (l) 
                                                              (*setRef/k l  v k)))))]

    ;[id (s) (denotable->expressible(lookupEnv s env))]
     
  ;    [id1 (s) (let ([val (denotable->expressible(lookupEnv s env))])
  ;                (if (procedure? val)
 ;                       (apply val '())
;                  val))]
      
      [id (s) (lookupEnv/k s env
			  (lambda(v)
			    (let ((val (denotable->expressible v))) (if (procedure? val) (apply val (list k)) (applyk k val))
                             )
                            ))]

;    [primApp (s rands) (letrec ((proc   (op s))
;             (args   (map (lambda(u)(eval-ast u env)) rands)))   
;			(apply proc args))]
     [primApp (s rands) (letrec ([proc   (op s)])
                        (map/k (lambda(u k)(eval-ast/k u env k))
                               rands
                               (lambda(args) (k (apply proc args)))))] 

;   [applyf (fid rands)(letrec ((c (eval-ast fid env)))
;			(apply-closure/k c rands env))]
    [applyf (fid rands) (eval-ast/k fid env (lambda(c) (apply-closure/k c rands env k)))]

;   [assume (binds body)   
;        (let ([tpls (map  (lambda(u)   (mk-tuple  
;                                              (first u)  
;                                              (expressible->denotable 
;                                                   (eval-ast (second u) env))))
;                      binds)]) ;evaluate asts to get values to be bound to identifiers              
;              ;evaluate the body in the extended environment
;	  (eval-ast body (extended-env tpls env)))]

    [assume (binds body)   
	    (map/k  (lambda(u k)
		      (eval-ast/k  (second u)
			  	   env
				   (lambda(v)
				   (k (mk-tuple (first u) (expressible->denotable  v))))))
	      binds
	      (lambda (tpls)
             	    (eval-ast/k body
			      (extended-env tpls env)
			      k)))] 

 ;   [assume* (binds body)   
 ;        (letrec (
                  ;; all evaluated tuples
 ;                 [tplsAll (map  (lambda(u)  (mk-tuple 
 ;                                                 (first u) 
 ;                                                   (expressible->denotable 
 ;                                                      (eval-ast (second u) env))))
 ;                         binds)]
 ;
 ;                 ;; all tuples which have closures build recursive environment
 ;                 [rec-tuples (filter (lambda(u) (functionHandle? 
 ;                                                   (denotable->expressible (second u))))
 ;                               tplsAll)]


                  ;; make all tuples with closures recursive
  ;                [recTplsAll   (map  (lambda(u)
 ;                                      (mk-rec-tuple (first u) (second u) rec-tuples))
 ;                                    tplsAll)])
                         ;evaluate the body in the extended environment
 ;                        (eval-ast body (extended-env recTplsAll
 ;					      env)))]
     [assume* (binds body)   
	 (map/k  (lambda(u k) (eval-ast/k (second u)
					env
					(lambda(v) (k (mk-tuple (first u)  (expressible->denotable v))))))
	         binds
	         (lambda (tplsAll)
	          (letrec (;; all tuples which have closures build recursive environment
			   [closure-tuples (filter (lambda(u)
						    (functionHandle? 
                                                    (denotable->expressible (second u)))) tplsAll)])

                             ;; make all tuples with closures recursive
                           (map/k  (lambda(u k) (k (mk-rec-tuple (first u) (second u) closure-tuples))) 
                                   tplsAll
			           (lambda (recTplsAll)
                                             ;evaluate the body in the extended environment
			                   (eval-ast/k body
				                     (extended-env recTplsAll env)
				                     k))))))]
	    
     [assume& (binds body)   
	 (map/k  (lambda(u k) (eval-ast/k (second u)
					env
					(lambda(v) (k (mk-tuple (first u)  (expressible->denotable v))))))
	         binds
	         (lambda (tplsAll)
	          (letrec (;; all tuples which have closures build recursive environment
			   [closure-tuples (filter (lambda(u)
						    (functionHandle? 
                                                    (denotable->expressible (second u)))) tplsAll)])

                             ;; make all tuples with closures recursive
                           (map/k  (lambda(u k) (k (mk-rec-tuple (first u) (second u) closure-tuples))) 
                                   tplsAll
			           (lambda (recTplsAll)
                                             ;evaluate the body in the extended environment
			                   (eval-ast/k body
				                     (extended-env recTplsAll env)
				                     k))))))]
     [assume-v (binds body)   
	 (map/k  (lambda(u k) (eval-ast/k (second u)
					env
					(lambda(v) (k (mk-tuple (first u)  (expressible->denotable v))))))
	         binds
	         (lambda (tplsAll)
	          (letrec (;; all tuples which have closures build recursive environment
			   [closure-tuples (filter (lambda(u)
						    (functionHandle? 
                                                    (denotable->expressible (second u)))) tplsAll)])

                             ;; make all tuples with closures recursive
                           (map/k  (lambda(u k) (k (mk-rec-tuple (first u) (second u) closure-tuples))) 
                                   tplsAll
			           (lambda (recTplsAll)
                                             ;evaluate the body in the extended environment
			                   (eval-ast/k body
				                     (extended-env recTplsAll env)
				                     k))))))]
      
 ; [seq (exps) (letrec ([args (map (lambda(u)(eval-ast u env)) exps)]
 ;                      [n    (length args)])
 ;              	 (list-ref args (- n 1)))]

  [seq (exps) (map/k (lambda(u k)(eval-ast/k u env k))
                       exps
                      (lambda(args)
                        (k (list-ref args (- (length args) 1)))))]
                               

  

 ) ;end of cases
  ); end of lambda
 ); end of eval-ast/k

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

