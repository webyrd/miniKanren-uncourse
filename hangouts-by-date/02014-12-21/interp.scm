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



(define my-append
  (lambda (l s)
    (cond
      [(null? l) s]
      [else (cons (car l) (my-append (cdr l) s))])))

(my-append '(a b c) '(d e))
;; =>
; (a b c d e)


(define appendo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))])))

(run* (q) (appendo '(a b c) '(d e) q))
;; =>
; ((a b c d e))

(run* (q) (appendo q '(d e) '(a b c d e)))
;; =>
; ((a b c))


(define my-append
  (lambda (l s)
    (if (null? l)
        s
        (cons (car l) (my-append (cdr l) s)))))

(my-append '(a b c) '(d e))
;; =>
; (a b c d e)


;; Curried my-append
;;
;; extend interpter with: 'if', 'cons', 'car', 'cdr', 'null?'
(define my-append
  (lambda (l)
    (lambda (s)
      (if (null? l)
          s
          (cons (car l) ((my-append (cdr l)) s))))))

((my-append '(a b c)) '(d e))
;; =>
; (a b c d e)

(letrec ((my-append
          (lambda (l)
            (lambda (s)
              (if (null? l)
                  s
                  (cons (car l) ((my-append (cdr l)) s)))))))
  ((my-append '(a b c)) '(d e)))
;; =>
; (a b c d e)

;; Y X = X (Y X)
(define Y
  (lambda (f)
    ((lambda (x)
       (f (x x)))
     (lambda (x)
       (lambda (y) ((f (x x)) y))))))

;; eta expansion

;; add1
;; ==
;; (lambda (x) (add1 x))

;; (add1 5) => 6
;; ((lambda (x) (add1 x)) 5) => 6


(((Y (lambda (my-append)
       (lambda (l)
         (lambda (s)
           (if (null? l)
               s
               (cons (car l) ((my-append (cdr l)) s)))))))
  '(a b c))
 '(d e))


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
      [(cons ,e1 ,e2)
       (cons (eval-exp e1 env) (eval-exp e2 env))]
      [(car ,e)
       (car (eval-exp e env))]
      [(cdr ,e)
       (cdr (eval-exp e env))]
      [(null? ,e)
       (null? (eval-exp e env))]
      [(if ,t ,c ,a)
       (if (eval-exp t env)
           (eval-exp c env)
           (eval-exp a env))]
      [(list . ,expr*)
       (eval-exp* expr* env)]
      [(lambda (,x) ,body) ;; abstraction
       `(closure ,x ,body ,env)]
      [(,e1 ,e2) ;; application
       (let ((proc (eval-exp e1 env))
             (val (eval-exp e2 env)))
         (pmatch proc
           [(closure ,x ,body ,envˆ)
            (eval-exp body `((,x . ,val) . ,envˆ))]
           [,else (error 'eval-exp "e1 does not evaluate to a procedure")]))]
      [,else (printf "failed to match against ~s\n" else)])))

(define my-append-call
  '((((lambda (f)
        ((lambda (x)
           (f (x x)))
         (lambda (x)
           (lambda (y) ((f (x x)) y)))))
      (lambda (my-append)
        (lambda (l)
          (lambda (s)
            (if (null? l)
                s
                (cons (car l) ((my-append (cdr l)) s)))))))
     (quote (a b c)))
    (quote (d e))))

(eval-exp my-append-call '())

(eval-exp '(cons (quote a) (quote b)) '())
(eval-exp '(car (cons (quote a) (quote b))) '())
(eval-exp '(cdr (cons (quote a) (quote b))) '())
(eval-exp '(null? (cons (quote a) (quote b))) '())
(eval-exp '(null? (quote ())) '())
(eval-exp '(if (null? (quote ())) (quote true) (quote false)) '())
(eval-exp '(if (null? (cons (quote a) (quote b))) (quote true) (quote false)) '())






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
        [(fresh (e1 e2 v1 v2)
           (== `(cons ,e1 ,e2) expr)
           (== `(,v1 . ,v2) out)
           (unboundo 'cons env)
           (eval-expo e1 env v1)
           (eval-expo e2 env v2))]
        [(fresh (e v2)
           (== `(car ,e) expr)
           (unboundo 'car env)
           (eval-expo e env `(,out . ,v2)))]
        [(fresh (e v1)
           (== `(cdr ,e) expr)
           (unboundo 'cdr env)
           (eval-expo e env `(,v1 . ,out)))]
        [(fresh (e v)
           (== `(null? ,e) expr)
           (conde
             [(== '() v) (== #t out)]
             [(=/= '() v) (== #f out)])
           (unboundo 'null? env)
           (eval-expo e env v))]
        [(fresh (t c a b)
           (== `(if ,t ,c ,a) expr)
           (unboundo 'if env)
           (eval-expo t env b)
           (conde
             [(== #f b) (eval-expo a env out)]
             [(=/= #f b) (eval-expo c env out)]))]
        [(fresh (expr*)
           (== `(list . ,expr*) expr)
           (unboundo 'list env)
           (eval-exp*o expr* env out))]
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

(define my-append-call
  '((((lambda (f)
        ((lambda (x)
           (f (x x)))
         (lambda (x)
           (lambda (y) ((f (x x)) y)))))
      (lambda (my-append)
        (lambda (l)
          (lambda (s)
            (if (null? l)
                s
                (cons (car l) ((my-append (cdr l)) s)))))))
     (quote (a b c)))
    (quote (d e))))


(run* (q) (eval-expo my-append-call '() q))
;; =>
; ((a b c d e))

(run 1 (q) (eval-expo
            `((((lambda (f)
                  ((lambda (x)
                     (f (x x)))
                   (lambda (x)
                     (lambda (y) ((f (x x)) y)))))
                (lambda (my-append)
                  (lambda (l)
                    (lambda (s)
                      (if (null? l)
                          s
                          (cons (car l) ((my-append (cdr l)) s)))))))
               (quote (a b c)))
              ,q)
            '()
            '(a b c d e)))

(run 1 (q) (eval-expo
            `((((lambda (f)
                  ((lambda (x)
                     (f (x x)))
                   (lambda (x)
                     (lambda (y) ((f (x x)) y)))))
                (lambda (my-append)
                  (lambda (l)
                    (lambda (s)
                      (if (null? l)
                          s
                          (cons (car l) ((my-append (cdr l)) s)))))))
               (quote (a b c)))
              ,q)
            '()
            '(a b c d e)))
;; =>
;; ('(d e))

(run 2 (q) (eval-expo
            `((((lambda (f)
                  ((lambda (x)
                     (f (x x)))
                   (lambda (x)
                     (lambda (y) ((f (x x)) y)))))
                (lambda (my-append)
                  (lambda (l)
                    (lambda (s)
                      (if (null? l)
                          s
                          (cons (car l) ((my-append (cdr l)) s)))))))
               (quote (a b c)))
              ,q)
            '()
            '(a b c d e)))
;; =>
;; ('(d e) ((car '((d e) . _.0)) (absento (closure _.0))))

(run* (q) (eval-expo
            `((((lambda (f)
                  ((lambda (x)
                     (f (x x)))
                   (lambda (x)
                     (lambda (y) ((f (x x)) y)))))
                (lambda (my-append)
                  (lambda (l)
                    (lambda (s)
                      (if (null? l)
                          s
                          (cons (car l) ((my-append (cdr l)) s)))))))
               (quote (a b c)))
              (quote ,q))
            '()
            '(a b c d e)))
;; =>
; ((d e))

(run* (x y) (eval-expo
            `((((lambda (f)
                  ((lambda (x)
                     (f (x x)))
                   (lambda (x)
                     (lambda (y) ((f (x x)) y)))))
                (lambda (my-append)
                  (lambda (l)
                    (lambda (s)
                      (if (null? l)
                          s
                          (cons (car l) ((my-append (cdr l)) s)))))))
               (quote ,x))
              (quote ,y))
            '()
            '(a b c d e)))
;; =>
;; ((() (a b c d e))
;;  ((a) (b c d e))
;;  ((a b) (c d e))
;;  ((a b c) (d e))
;;  ((a b c d) (e))
;;  ((a b c d e) ()))


;(eval-exp '(cons (quote a) (quote b)) '())
;(eval-exp '(car (cons (quote a) (quote b))) '())
;(eval-exp '(cdr (cons (quote a) (quote b))) '())
;(eval-exp '(null? (cons (quote a) (quote b))) '())
;(eval-exp '(null? (quote ())) '())
;(eval-exp '(if (null? (quote ())) (quote true) (quote false)) '())
;(eval-exp '(if (null? (cons (quote a) (quote b))) (quote true) (quote false)) '())
