
#lang racket
(provide (all-defined-out))
(require eopl)
(require "ast.rkt" "env.rkt" "parser.rkt" )
;(require racket/trace)
;(require rackunit)
;(require rackunit/text-ui)

(define applyk (lambda (k v)
                  (k v)))

;(define map (lambda(f ls)
;              (if (null? ls) '()
;                  (cons (f (car ls)) (map f (cdr ls))))))

(define map/k (lambda(f ls k)
              (begin
               (if (null? ls) (k '())
                  (f (car ls) (lambda(v)
                                (map/k f (cdr ls) (lambda (w)(k (cons v w))))))))))

;(define call-by-value (lambda (rands env)
;                             (map 
;                                 (lambda (x) 
;                                   (expressible->denotable 
;                                         (eval-ast x env)))
;                                 rands)))
(define make-call-by-name/k (lambda (rands env)
                          (begin  (map 
                                 (lambda (x) 
                                         (expressible->denotable (expargu x env)))
                                 rands
				 ))))

(define call-by-value/k (lambda (rands env k)
                          (map/k 
                                 (lambda (x k) 
                                         (eval-ast/k x env  (lambda(v) (k (expressible->denotable v)))))
                                 rands
				 k)))

(define find-call-by-name/k (lambda (argp)
                          (if (not (Argus? argp))  (begin argp)
                          (begin (begin (cases Argus argp
                          [expargu (asp env)
                                   (let ((k (lambda(g) g)))
                                   (eval-ast/k asp env  (lambda(v) (k v)))
                                   )
                          ]
                            ))))))


(define apply-closure/k (lambda (c rands envOuter k)
(cases functionHandle c

    [rec-closure-v (formals body v)
	    (make-call-by-name/k rands envOuter (lambda (args)
					         (eval-ast/k body  (extended-env (mk-tuples formals args) (vector-ref v 0)) k)))]
   [rec-closure (formals body env r)
				(map/k (lambda (x k)  (k (mk-rec-tuple (first x) (second x) r)))
							   r
							   (lambda(rr)
								 (eval-ast/k body
									   (extended-env rr  (extended-env (mk-tuples formals (make-call-by-name/k rands envOuter)) env))
									   k))) ]

   [closure (formals body env)   (begin 
	    ;;(make-call-by-name/k rands envOuter) ;;(lambda (args)
                                                 (begin 
					         (eval-ast/k body  (extended-env (mk-tuples formals (make-call-by-name/k rands envOuter)) env) k)))]

   (else (error 'not_a_function_handle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;eval-ast/k : [Ast? env? procedure?]--> ans? or error
;; throws error for arity mismatch and unbound identifier
(define eval-ast/k
  (lambda (ast env k)
    (if (Argus? ast) (find-call-by-name/k ast k)
     (cases Ast ast 

      [num (n)  (applyk k n)]
      
      [bool(b) (applyk k b)]
     
      [function (formals body)    (applyk k (closure formals body env))]

     ;[ifte (test then else) (let ((b (eval-ast test env)))
     ;                             (cond
     ;                             [(boolean? b) (if b (eval-ast then env)
     ;                                                 (eval-ast else env))]
     ;                             (else (error 'eval-ast "ifte:test condition must be boolean instead of ~a" b))))]
     [ifte (test then else) (begin                           
           (eval-ast/k test env
				       (lambda (b)
                                         (begin 
                                          (if (boolean? b)
					    (if b
					       (eval-ast/k then env k)
                                               (eval-ast/k else env k))
                                         (else (error 'eval-ast/k "ifte:test condition must be boolean instead of ~a" b)))))))]

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
     
     [id (s) (begin (lookupEnv/k s env
			  (lambda(v)
                            (begin
			    (k  (find-call-by-name/k (denotable->expressible v)))))))]

;    [primApp (s rands) (letrec ((proc   (op s))
;             (args   (map (lambda(u)(eval-ast u env)) rands)))   
;			(apply proc args))]
     [primApp (s rands) (begin (letrec ([proc   (op s)])
                        (map/k (lambda(u k)(eval-ast/k u env k))
                               rands
                               (lambda(args) (k (apply proc args))))))] 

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
          (letrec (
                   ;; all evaluated tuples
                   [tplsAll (map  (lambda(u)  (mk-tuple
                                                   (first u)
                                                    (expressible->denotable
                                                        (eval-ast/k (second u) env))))
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
                (eval-ast/k body (extended-env vtplsAll env)))] 

      
 ; [seq (exps) (letrec ([args (map (lambda(u)(eval-ast u env)) exps)]
 ;                      [n    (length args)])
 ;              	 (list-ref args (- n 1)))]

  [seq (exps) (map/k (lambda(u k)(eval-ast/k u env k))
                       exps
                      (lambda(args)
                        (k (list-ref args (- (length args) 1)))))]
                               

  

 );end of cases
)
  ); end of lambda
 ); end of eval-ast/k

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
