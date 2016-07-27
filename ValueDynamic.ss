


;;---------------------------------------------------------------------
;;
;;  C A L L -- B Y -- V A L U E   ,    D Y N A M I C    S C O P I N G
;;
;;                       Sairama Rachakonda
;;---------------------------------------------------------------------

#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define name?
  (lambda (s)
    (and (symbol? s)
         (not (memq s '(if lambda))))))

(define andmap
  (lambda (f l)
    (if (null? l) 
	#t
	(and (f (car l))
	     (andmap f (cdr l))))))


(define check1

  (lambda (l) (
                ;;l=(variable init)
                ;;check if (car l) is name or not
               if (and
                       (list? l)
                       (= 2 (length l))
                       (name? (car l))
                       )
                  #t #f
                 )) )

(define getVars
  (lambda (l) (
               ;;l = (variable init)
               ;;parse the init
              ;; write (cons (parse (car l)) (parse (cadr l)))
              ;; write (cons 1 2)
                ;; cons (parse (car l)) (parse (cadr l))
               car l
               
               )))

(define getExprs
  (lambda (l) (
                 parse(cadr l)
               )))


(define parse
  (lambda (m)
    (cond
      ((number? m)  `(&const ,m))
      ((eq? #t m)   `(&const #t))
      ((eq? #f m)   `(&const #f))
      ((name? m)    `(&var ,m))
      ((pair? m)    (cond 
                      ((eq? `if (car m))
                        (if (= 4 (length m))
                            `(&if ,(parse (cadr m))
                                  ,(parse (caddr m)) ,(parse (cadddr m)))
                            (error 'parse "Syntax error")))
                      ((eq? `lambda (car m))
                        (if (and (= 3 (length m))
                                 (list? (cadr m))
                                 (andmap name? (cadr m)))
                            `(&lambda ,(cadr m) ,(parse (caddr m)))
                            (error 'parse "Syntax error")))
                       ((eq? `let (car m))               ;; (let (()...) body)
                          (if (and (<= 3 (length m))
                                   (list? (cadr m))
                                   (andmap check1 (cadr m) )   )
                             `(&let ,(map getVars (cadr m)),(map getExprs (cadr m)),(map parse (cddr m)))   ;;(&let (ids) (args) bodyList)
                              (write 'Error!!)   )
                       )
                       ((eq? `let* (car m))               ;; (let* (()...) body)
                          (if (and (<= 3 (length m))
                                   (list? (cadr m))
                                   (andmap check1 (cadr m) )   )
                             `(&let* ,(map getVars (cadr m)),(map getExprs (cadr m)),(map parse (cddr m))) ;;(&let* (ids) (args) bodyList)
                              (write 'Error!!)   )
                       )
                      (else
                        `(&apply ,(parse (car m)) ,(parse* (cdr m))))))
      (else         (error 'parse "Syntax error")))))




(define parse* (lambda (m) (map parse m)))

(define unparse-let-utility
  (lambda (ids exprs combinedList)
    (cond
       ((and (null? ids) (null? exprs)) combinedList)
       ((eq? combinedList '())  (unparse-let-utility (cdr ids) (cdr exprs) (cons (car ids) (cons (unparse (car exprs)) '()) )))
       (else  (unparse-let-utility (cdr ids) (cdr exprs) (list combinedList (cons (car ids) (cons (unparse (car exprs))'())))))                       
      )
    
))

(define unparse
  (lambda (a)
    (cond
      ((eq? (car a) `&const) (cadr a))
      ((eq? (car a) `&var)   (cadr a))
      ((eq? (car a) `&if)    `(if ,(unparse (cadr a)) ,(unparse (caddr a)) 
                                 ,(unparse (cadddr a))))
      ((eq? (car a) '&lambda) `(lambda ,(cadr a) ,(unparse (caddr a))))
      ((eq? (car a) `&apply)  (cons (unparse (cadr a)) (map unparse (caddr a))))
      ((eq? (car a) '&closure) `(lambda ,(caddr a) ,(unparse (cadddr a))))
      ((eq? (car a) '&let) `(let ,(unparse-let-utility (cadr a) (caddr a) '()) ,(unparse (car (cadddr a))) ))
      ((eq? (car a) '&let*) `(let* ,(unparse-let-utility (cadr a) (caddr a) '()) ,(unparse (car (cadddr a))) ))
      (else  (error 'unparse "unexpected syntax tree" a)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Environment and closure representations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; environments, functional implementation
(define extend*
  (lambda (env xs vs)
    (if (null? xs)
        env
        (extend* (extend env (car xs) (car vs)) (cdr xs) (cdr vs)))))

;;(define extend
 ;; (lambda (env x v)
   ;; (lambda (y)
     ;; (cond ((eq? x y) v)
       ;;   (else (lookup env y))))))

 (define extend
   (lambda (env x v)

      (cons (list x v) env) 
   )
 )

;;(define interpret-free-var
  ;;(lambda (x)
    ;;`(&const ,(eval x))))

;;(define empty-env interpret-free-var)

;;(define lookup
 ;; (lambda (env y) (env y)))

(define empty-env '())

;;(define lookup
;;   (lambda (env y) 

 ;;    (cond ((eq? env empty-env) `(&const ,(eval y)))
 ;;          ((eq? (caar env) y) (cadr (car env)))
 ;;          (else  (lookup (cdr env) y))

 ;;  ))
;;)


(define lookup
   (lambda (env y) 

     (cond ((eq? '+ y) `(&const ,(eval y)))
           ((eq? '- y) `(&const ,(eval y)))
           ((eq? '* y) `(&const ,(eval y)))
           ((eq? '/ y) `(&const ,(eval y)))
           ((eq? 'equal? y) `(&const ,(eval y)))
           ((eq? env empty-env) `(&const ,(eval y)))
           ((eq? (caar env) y) (cadr (car env)))
           (else  (lookup (cdr env) y))

   ))
)


;; closures, list implementation 
(define mk-closure                     ;; returns (&closure env parm-list body)
  (lambda (env v)
    (cond
     ((eq? (car v) '&lambda) `(&closure ,env ,(cadr v) ,(caddr v)))))) 

(define apply-cl
  (lambda (vf va env)
    (cond
     ((eq? (car vf) '&closure)
      (let ( ;;(env (cadr vf))      ;; environment
	    (p   (caddr vf))     ;; parameter list
	    (b   (cadddr vf)))   ;; body
        (if (= (length p) (length va))
            (ev b (extend* env p va))
            (error 'apply-cl "wrong number of arguments")))))))

(define delta
  (lambda (f a)
    (let ((R (lambda (s) `(&const ,s)))
          (R-1 (lambda (cl)
                 (cond
		  ((eq? (car cl) `&const) (cadr cl))
		  (else (error 'delta "non-constant argument"))))))
      (R (apply (R-1 f) (map R-1 a))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Closure Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define evalArgs

  (lambda (args env)

     (map (lambda (a) (ev a env)) args)

    ))

(define lastElement (lambda (l) 
                          
    (cond ((null? (cdr l)) (car l))
       (else (lastElement (cdr l))))
))



(define evalBodyList (lambda (bodyList env) 

   (map (lambda(b) (ev b env))  bodyList)
  
))

(define evalArgList* (lambda(idList argList env)

   (if (null? argList)
       env
       (evalArgList* (cdr idList) (cdr argList) (extend* env (list (car idList)) (list (ev (car argList) env))))
   )    

))

(define ev
  (lambda (e env)
    (cond
      ((eq? (car e) '&const) e)                            ;; e == (&const c)
      ((eq? (car e) '&var) (lookup env (cadr e)))          ;; e == (&var v)
      ((eq? (car e) '&lambda)  (mk-closure env e))         ;; e == (&lambda parm-list body)
      ((eq? (car e) '&if) (let ((a (cadr e))               ;; e == (&if a b c)
				(b (caddr e))
				(c (cadddr e)))
			    (ev (if (equal? (ev a env) '(&const #f)) c b) env)))
      ((eq? (car e) '&let) (let* ((formalParams (cadr e))
                                 (actualArgs (caddr e))
                                 (body (cadddr e))
                                 (extEnv (extend* env formalParams (evalArgs actualArgs env)))  )
                                 (lastElement  (evalBodyList body extEnv))))
                                 
      
       ((eq? (car e) '&let*) (let* ((formalParams (cadr e))
                                 (actualArgs (caddr e))
                                 (body (cadddr e))
                                 (extEnv (evalArgList* formalParams actualArgs env))  )
                                 (lastElement  (evalBodyList body extEnv))))

      ((eq? (car e) `&apply) (let ((f (cadr e))            ;; e == (&apply f args)
				   (args (caddr e)))
			       (let ((fv (ev f env))
				     (av (map (lambda (a) (ev a env)) args)))
				 (if (and (pair? fv) (eq? (car fv) '&const))
				     (delta fv av)
				     (apply-cl fv av env))))))))


(define evaluate
  (lambda (m)
    (unparse (ev (parse m) empty-env))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Test cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M1 `((lambda (x) (+ x 1)) 5))
(define M2 `((lambda (x) (+ x 2)) (if #t 3 2)))
(define M3 `(lambda (x y z) ((x y) z))) 
(define M4 '((lambda (y) (y 1 2)) (lambda (x z) (+ x z))))
(define M5 '((lambda (x) ( (lambda (x) (+ x 1)) 2 ) ) 1))
(define M6 `((lambda (x z) (x 1 (z 2 2))) + *))
(define M7 `((lambda () 1)))
(define M8 `((lambda (x) (+ x 0)) (+ 1 2)))
(define M9 '((lambda (x) x) (+ 1 2)))
(define M10 '((lambda (x) ((lambda (z) ((lambda (x) (z x)) 3)) (lambda (y) (+ x y)))) 1))
(define M11 '((lambda (x) 1) ((lambda (x) (x x)) (lambda (x) (x x)))))
(define M20 '( (lambda (x) ((lambda (z) ((lambda (x) (+ z x)) 2)) x)) 1))
(define M21 '((lambda (x y) (let ((a x) (b y)) (+ a b))) 1 2))

(define M22 '((lambda (z) (let ((x 5))
    (let ((x 2)
          (y x))
      (list y x))))1) )

(define M23 '((lambda (z) (let ((a 1) (b 2)) (+ a b) (* a b))) 3))

(define M24 '((lambda (z) (let* ([x 1]
         [y (+ x 1)])
    (list y x))) 3))

(define Fact ((lambda (x)

           (let fac ([n x])
    (if (zero? n)
        1
        (* n (fac (sub1 n)))))                

                ) 10))


;; computation does not terminate!
(define test-wrong
'(((lambda (f)
        ((lambda (x) (f (x x)))
         (lambda (x) (f (x x)))))
    (lambda (fac)
      (lambda (x)
        (if (equal? x 1) 1 (* x (fac (- x 1))))))) 3))


;; computes 3!
(define test-fac3
'(((lambda (f)
    ((lambda (x) (lambda (y) ((f (x x)) y)))
     (lambda (x) (lambda (y) ((f (x x)) y)))))
    (lambda (fac)
      (lambda (x)
        (if (equal? x 1) 1 (* x (fac (- x 1))))))) 3))

;; computes 5!
(define test-fac5
'(((lambda (f)
    ((lambda (x) (lambda (y) ((f (x x)) y)))
     (lambda (x) (lambda (y) ((f (x x)) y)))))
    (lambda (fac)
      (lambda (x)
        (if (equal? x 1) 1 (* x (fac (- x 1))))))) 5))


