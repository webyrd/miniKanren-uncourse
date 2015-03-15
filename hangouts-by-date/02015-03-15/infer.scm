(load "../../mk-implementations/scheme/mk.scm")

;;; simple type inferencer for a Scheme-like language

;;; from miniKanren uncourse hangout #16, 15 March 02105

(define lookupo
  (lambda (x gamma type)
    (fresh (y t rest)
      (== `((,y . ,t) . ,rest) gamma)
      (conde
        [(== x y) (== t type)]
        [(=/= x y)
         (lookupo x rest type)]))))

(define !-
  (lambda (gamma expr T)
    (conde
      [(== #f expr) (== 'bool T)]      
      [(== #t expr) (== 'bool T)]
      [(numbero expr) (== 'int T)]
      [(symbolo expr) ; variable x
       (lookupo expr gamma T)]
      [(fresh (e)
         ;; int -> bool
         (== `(zero? ,e) expr)
         (== 'bool T)
         (!- gamma e 'int))]
      [(fresh (e1 e2)
         (== `(- ,e1 ,e2) expr)
         (== 'int T)
         (!- gamma e1 'int)
         (!- gamma e2 'int))]
      [(fresh (e1 e2)
         (== `(* ,e1 ,e2) expr)
         (== 'int T)
         (!- gamma e1 'int)
         (!- gamma e2 'int))]
      [(fresh (e1 e2 e3)
         (== `(if ,e1 ,e2 ,e3) expr)
         (!- gamma e1 'bool)
         (!- gamma e2 T)
         (!- gamma e3 T))]
      [(fresh (x e T1 T2) ; lambda
         (== `(lambda (,x) ,e) expr)
         (== `(,T1 -> ,T2) T)
         (!- `((,x . ,T1) . ,gamma) e T2))]
      [(fresh (e1 e2 T1) ; application
         (== `(,e1 ,e2) expr)
         (!- gamma e1 `(,T1 -> ,T))
         (!- gamma e2 T1))])))
