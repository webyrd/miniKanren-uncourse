;; miniKanren Hangout #6: extending the Relational Scheme interpreter to handle list-related functions in Scheme
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


(define eval-exp*
  (lambda (expr* env)
    (cond
      [(null? expr*) '()]
      [else (cons
              (eval-exp (car expr*) env)
              (eval-exp* (cdr expr*) env))])))

(define eval-exp
  (lambda (expr env)
    (pmatch expr
      [,x (guard (symbol? x)) ;; variable
       (lookup x env)]
      [(quote ,datum) datum]
      [(list . ,expr*)
       ;; (map (lambda (expr) (eval-exp expr env)) expr*)
       (eval-exp* expr* env)]
      [(lambda (,x) ,body) ;; abstraction
       `(closure ,x ,body ,env)]
      [(,e1 ,e2) ;; application
       ;; eval e1 (better evaluate to a closure!) -> proc
       ;; eval e2 -> val
       ;; apply proc to val
       (let ((proc (eval-exp e1 env))
             (val (eval-exp e2 env)))
         (pmatch proc
           [(closure ,x ,body ,envˆ)
            ;; evaluate body in an extended environment
            ;; in which the environment of the closure is extended
            ;; with a binding between x and val
            (eval-exp body `((,x . ,val) . ,envˆ))]
           [,else (error 'eval-exp "e1 does not evaluate to a procedure")]))])))

(eval-exp '(quote 5) '())

(eval-exp '(list ((lambda (x) x) (quote 5)) (quote 6) ((lambda (y) (lambda (z) y)) (quote 7))) '())



;; call-by-value environment-passing lambda-calculus interpreter in miniKanren

;; env : mapping from symbol (variable) to value
;;
;; (lookupo 'y '((x . 5) (y . (#t foo))) '(#t foo))

(define lookupo
  (lambda (x env out)
    (fresh (y val envˆ)
      (== `((,y . ,val) . ,envˆ) env)
      (symbolo x)
      (symbolo y)
      (conde
        [(== x y) (== val out)]
        [(=/= x y) (lookupo x envˆ out)]))))

(run 1 (q) (lookupo 'y '((x . 5) (y . 6)) q))

(run 2 (q) (lookupo 'x '((x . 5) (y . 6) (x . 7)) q))
;; => (5)

(run* (q) (lookupo q '((x . 5) (y . 6)) '5))
;; => (x)




(define unboundo
  (lambda (x env)
    (fresh ()
      (symbolo x)
      (conde
        [(== '() env)]
        [(fresh (y v env^)
           (== `((,y . ,v) . ,env^) env)
           (=/= x y)
           (unboundo x env^))]))))

(define eval-expo
  (lambda (expr env out)
    (fresh ()
      (conde
        [(symbolo expr) ;; variable
         (lookupo expr env out)]
        [(== `(quote ,out) expr)
         (absento 'closure out)
         (unboundo 'quote env)]
        [(fresh (expr*)
           (== `(list . ,expr*) expr)
           (eval-exp*o expr* env out)
           (unboundo 'list env))]
        [(fresh (x body) ;; abstraction
           (== `(lambda (,x) ,body) expr)
           (== `(closure ,x ,body ,env) out)
           (symbolo x)
           (unboundo 'lambda env))]
        [(fresh (e1 e2 val x body envˆ) ;; application
           (== `(,e1 ,e2) expr)
           (eval-expo e1 env `(closure ,x ,body ,envˆ))
           (eval-expo e2 env val)
           (eval-expo body `((,x . ,val) . ,envˆ) out))]))))

(define eval-exp*o
  (lambda (expr* env out)
    (conde
      [(== '() expr*) (== '() out)]
      [(fresh (a d res-a res-d)
         (== (cons a d) expr*)
         (== (cons res-a res-d) out)
         (eval-expo a env res-a)
         (eval-exp*o d env res-d))])))


(run* (q) (eval-expo '(list ((lambda (x) x) (quote 5)) (quote 6) ((lambda (y) (lambda (z) y)) (quote 7))) '() q))

(run 1 (q) (eval-expo q '() '(5 6 (closure z y ((y . 7))))))


(run* (q) (eval-expo '((lambda (x) x) (lambda (y) y)) '() q))

(run* (q) (eval-expo '((lambda (closure) closure) (lambda (y) y)) '() q))

(run* (q) (eval-expo '((lambda (quote) quote) (lambda (y) y)) '() q))


(run* (q) (eval-expo '((lambda (x) (x x)) (lambda (y) y)) '() q))

(run* (q) (eval-expo '((lambda (quote) (quote quote)) (lambda (y) y)) '() q))


(run* (q) (eval-expo '(lambda (x) x) '() q))
;; => ((closure x x ()))

(run* (q) (eval-expo '((lambda (y) y)
                       (lambda (x) x))
                     '()
                     q))
;; => ((closure x x ()))

(run 2 (q) (eval-expo q '() '(closure x x ())))



(run* (q) (eval-expo '((lambda (y)
                         (lambda (x) x))
                       (quote 5))
                     '()
                     q))
;; => ((closure x x ((y . 5))))

(run 1 (q) (eval-expo q '() '(5 6 (closure z y ((y . 7))))))

(run* (q) (eval-expo '(lambda (5) (lambda (y) y)) '() q)) ;; shows why we need a symbolo in the lambda clause
;; => ()

(run* (q) (eval-expo '(lambda (x) x) '() q))
;; => ((closure x x ()))

(run* (q) (eval-expo 'x '((x . 5)) q))
;; => (5)

(run* (q) (eval-expo '((lambda (x) x) (lambda (y) y)) '() q))
;; => ((closure y y ()))

(run* (q) (eval-expo '(((lambda (x) (lambda (x) x)) (lambda (y) y)) (lambda (w) w)) '() q))
;; => ((closure w w ()))

(run* (q) (eval-expo '(quote 5) '() q))
;; => (5)


;; 99 ways to say (I love you)
;; see also http://matt.might.net/articles/i-love-you-in-racket/
(run 99 (expr) (eval-expo expr '() '(I love you)))

;; Quines
(run 1 (q) (eval-expo q '() q))

;; Twines
(run 1 (p q)  ;; twines   twin quines
  (=/= p q)
  (eval-expo p '() q)
  (eval-expo q '() p))

;; Thrines
(run 1 (p q r) ;; thrines
  (=/= p q)
  (=/= p r)
  (=/= q r)
  (eval-expo p '() q)
  (eval-expo q '() r)
  (eval-expo r '() p))
;; =>
;; (((''((lambda (_.0)
;;         (list 'quote
;;           (list 'quote (list _.0 (list 'quote _.0)))))
;;        '(lambda (_.0)
;;           (list 'quote
;;             (list 'quote (list _.0 (list 'quote _.0))))))
;;     '((lambda (_.0)
;;         (list 'quote
;;           (list 'quote (list _.0 (list 'quote _.0)))))
;;        '(lambda (_.0)
;;           (list 'quote
;;             (list 'quote (list _.0 (list 'quote _.0))))))
;;     ((lambda (_.0)
;;        (list 'quote
;;          (list 'quote (list _.0 (list 'quote _.0)))))
;;       '(lambda (_.0)
;;          (list 'quote
;;            (list 'quote (list _.0 (list 'quote _.0)))))))
;;    (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
;;    (sym _.0)))


(define Y
  '(lambda (f)
     ((lambda (x)
        (f (x x)))
      (lambda (x)
        (f (x x))))))
