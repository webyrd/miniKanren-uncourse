;; miniKanren Hangout #4: Relational Scheme interpreter
(load "../../mk-implementations/scheme/mk.scm")
(load "../../mk-implementations/scheme/pmatch.scm")

;; call-by-value environment-passing lambda-calculus interpreter in Scheme

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

#|
;; metaphorical definition of lookup, with Dijkstra-style explicit guard
(define lookup
  (lambda (x env)
    (pmatch env
      [((,y . ,v) . ,envˆ) (guard (eq? x y))
       v]
      [()
       (error 'lookup "unbound variable")]
      [((,y . ,v) . ,envˆ) (guard (not (eq? x y)))
       (lookup x envˆ)])))
|#

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



;; call-by-value environment-passing lambda-calculus interpreter in miniKanren

;; env : mapping from symbol (variable) to value
;;
;; (lookupo 'y '((x . 5) (y . (#t foo))) '(#t foo))

(define lookupo
  (lambda (x env out)
    (fresh (y val envˆ)
      (== `((,y . ,val) . ,envˆ) env)
      (conde
        [(== x y) (== val out)]
        [(=/= x y) (lookupo x envˆ out)]))))

(run 1 (q) (lookupo 'y '((x . 5) (y . 6)) q))

(run 2 (q) (lookupo 'x '((x . 5) (y . 6) (x . 7)) q))
;; => (5)

(run* (q) (lookupo q '((x . 5) (y . 6)) '5))
;; => (x)

(define eval-expo
  (lambda (expr env out)
    (conde
      [(symbolo expr) ;; variable
       (lookupo expr env out)]
      [(fresh (x body) ;; abstraction
         (== `(lambda (,x) ,body) expr)
         (== `(closure ,x ,body ,env) out))]
      [(fresh (e1 e2 val x body envˆ) ;; application
         (== `(,e1 ,e2) expr)
         (eval-expo e1 env `(closure ,x ,body ,envˆ))
         (eval-expo e2 env val)
         (eval-expo body `((,x . ,val) . ,envˆ) out))])))

(run* (q) (eval-expo '(lambda (x) x) '() q))
;; => ((closure x x ()))

(run* (q) (eval-expo 'x '((x . 5)) q))
;; => (5)

(run* (q) (eval-expo '((lambda (x) x) (lambda (y) y)) '() q))
;; => ((closure y y ()))

(run* (q) (eval-expo '(((lambda (x) (lambda (x) x)) (lambda (y) y)) (lambda (w) w)) '() q))
;; => ((closure w w ()))

#|
(define eval-exp
  (lambda (expr env)
    (pmatch expr
      [,x (guard (symbol? x)) ;; variable
       (lookup x env)]
      [(lambda (,x) ,body) ;; abstraction
       `(closure ,x ,body ,env)]
      [(,e1 ,e2) ;; application
       (let ((proc (eval-exp e1 env))
             (val (eval-exp e2 env)))
         (pmatch proc
           [(closure ,x ,body ,envˆ)
            (eval-exp body `((,x . ,val) . ,envˆ))]
           [,else (error 'eval-exp "e1 does not evaluate to a procedure")]))])))
|#
