;; miniKanren Hangout #4: Relational Scheme interpreter
(load "../../mk-implementations/scheme/mk.scm")
(load "../../mk-implementations/scheme/pmatch.scm")

;; env : mapping from symbol (variable) to value
;; 
;; (lookup 'y '((x . 5) (y . (#t foo)))) => (#t foo)

(define lookup
  (lambda (x env)
    (pmatch env
      [() (error 'lookup "unbound variable")]
      [((,y . ,v) . ,envˆ) (guard (eq? x y))
       v]
      [((,y . ,_) . ,envˆ)
       (lookup x envˆ)])))

(define eval-exp
  (lambda (expr env)
    (pmatch expr
      [,x (guard (symbol? x)) ;; variable
       (lookup x env)]
      [(lambda (,x) ,body) ;; abstraction
       `(closure ,x ,body ,env)]
      [(,e1 ,e2) ;; application
       ;; eval e1 (better evaluate to a closure!) -> proc
       ;; eval e2 -> val
       ;; apply proc to val
       (let ((proc (eval-exp e1 env))
             (val (eval-exp e2 env)))
         (pmatch proc
           [(closure ,x ,body ,env)
            ;; evaluate body in an extended environment
            ;; in which the environment of the closure is extended
            ;; with a binding between x and val
            (eval-exp body `((,x . ,val) . ,env))]
           [,else (error 'eval-exp "e1 does not evaluate to a procedure")]))])))
