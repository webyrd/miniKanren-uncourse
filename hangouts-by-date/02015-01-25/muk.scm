;; microKanren in Scheme

;; based on 2013 Scheme Workshop paper by Jason Hemann and Dan Friedman
;; http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
;;
;; https://github.com/jasonhemann/microKanren

;; concepts in a miniKanren-like implementation
;; --------------------------------------------
;;
;; ability to create terms (numbers, pairs)
;;
;; logic variables
;;   ability to introduce (lexically scoped) fresh logic variables (fresh)
;;
;; unify terms
;;   ==
;;
;; conjunction [and] (fresh body/within a conde clause/run body)
;;
;; disjuction [or] (bewteen conde clauses)
;;
;; interface with the host language/query operator (run)
;;
;; display answers nicely (reification)



;; simplifying the language/implementation

(conde
  [g1 g2 g3]
  [g4]
  [g5 g6])


;; disjunction of conjunctions
(conde ; or
  [g1 g2 g3] ; and
  [g4] ; and
  [g5 g6] ;and
  )

(conde ; or
  [(fresh () g1 g2 g3)] ; and
  [(fresh () g4)] ; and
  [(fresh () g5 g6)] ;and
  )

(fresh () g1 g2 g3) ; and

(fresh (x y z) ; introduces fresh logic variables
  g1  ; and
  g2
  g3)

(run 1 (q)
  g1 g2 g3) ; and

(run 1
  (fresh (q)
    g1 g2 g3))


;; binary conjunction (and)
;; binary disjunction (or)

;; creation of a single fresh logic variable
;; scoping of a single logic variable using lambda from the host language

;; ==
;; unification


;; defer:
;; ------------------------
;; interface/query operator
;; reifying answers


(conde ; or
  [g1 g2 g3] ; and
  [g4] ; and
  [g5 g6] ;and
  )

(disj
  (conj g1
    (conj g2 g3))
  (disj
    g4
    (conj g5 g6)))


(fresh (x y z)
  g1 g2 g3)

(fresh-var (x)
  (fresh-var (y)
    (fresh-var (z)
      (conj g1
        (conj g2 g3)))))

(fresh-var (x)
  g1)


((lambda (x)
   ((lambda (y)
      (conj
       (== x 5)
       (== y 6)))
    (make-logic-var 1)))
 (make-logic-var 0))

(fresh (x y)
  (== x 5)
  (== y 6))


(fresh (x y)
  (== 6 x)
  (== 5 x) ; <- (== 5 6) fail
  )

;; substitution (unification)

;; start out with the empty substition
;; ()          empty list
;; ((x . 6))   extended substitution (association list)

(fresh (x y)
  (== x y))

;; ((x . y))
;;
;; or
;;
;; ((y . x))

(fresh (x y z)
  (== x `(,y ,z)))

;; ((x . (y z)))

;; substitution maps logic variables to terms

;; ((x . (y z)))

(== w 5)

;; ((w . 5) (x . (y z)))


;; triangular substitution

(fresh (x y z)
  ;; ()
  (== x y)
  ;; ((x . y))
  (== x 6)
  ;; ((y . 6) (x . y))
  )

(walk y `((,y . 6) (,x . ,y))) => 6
(walk x `((,y . 6) (,x . ,y))) => 6 ; (through an extra lookup)

(lookup x `((,y . 6) (,x . ,y))) => `(,x . ,y)
(lookup y `((,y . 6) (,x . ,y))) => `(,y . 6)

(walk x `((,w . ,v) (,z . ,w) (,y . ,z) (,x . ,y))) => v ;; representative element
;; variable chain x -> y -> z -> w -> v

`((,v . 5) (,w . ,v) (,z . ,w) (,y . ,z) (,x . ,y))

;; path compression could give us an idempotent substitution:

`((,v . 5) (,w . 5) (,z . 5) (,y . 5) (,x . 5) (,v . 5))


;; triangular substitutions vs. idempotent

;; triangular
(apply-subst `(,x ,y) `((,v . 5) (,w . ,v) (,z . ,w) (,y . ,z) (,x . ,y)))
=> `(,y ,z)
(apply-subt `(,y ,z) `((,v . 5) (,w . ,v) (,z . ,w) (,y . ,z) (,x . ,y)))
=> `(,z ,w)
...
(5 5)

;; idempotent
(apply-subst `(,x ,y) `((,v . 5) (,w . 5) (,z . 5) (,y . 5) (,x . 5) (,v . 5)))
=> (5 5)
(apply-subst `(5 5) `((,v . 5) (,w . 5) (,z . 5) (,y . 5) (,x . 5) (,v . 5)))
=> (5 5)

(fresh (x y z)
  ;; ()
  (== x y)
  ;; ((y . x))
  (== x 6)
  ;; ((x . 6) (y . x))
  )

;; property of our substitution: a logic variable appears on the
;; left-hand-side of a substitution at most once

;; extend

;; ((x . y))

;; with (y . 6) to get

;; ((y . 6) (x . y))


;; constraint store (more general notion)


;; Reasoned Schemer Code
;; https://github.com/miniKanren/TheReasonedSchemer

;; Cleaned up more modern simple miniKanren
;; https://github.com/miniKanren/simple-miniKanren

;; Efficient representations for triangular substitutions: A
;; comparison in miniKanren
;;
;; http://www.cs.indiana.edu/~lkuper/papers/walk.pdf


;; walk
;; substitutions: () or ((x . t1) (y . t2))

;; variable interface
;; (var 0) => make a new variable
;; (var? t) => #t or #f
;; (var=? v1 v2) =? #t or #f

(walk 5 `((,x . 7))) => 5
(walk y `((,x . 7))) => y
(walk `(,x ,y) `((,x . 5) (,y . 6))) => `(,x ,y)
(walk y `((,x . 5) (,y . 6))) => 6
(walk y `((,z . 5) (,y . ,z))) => 5

(define walk
  ;; walk term t in substitution s
  (lambda (t s)
    (cond
      ((var? t)
       (let ((pr (assq t s)))
         (if pr
             (walk (cdr pr) s)
             t)))
      (else t))))

(define walk
  (lambda (t s)
    (let ((pr (and (var? t) (assp (lambda (v) (var=? t v)) s))))
      (cond
        (pr (walk (cdr pr) s))
        (else t)))))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))


(fresh (x y)
  ; ()
  (== x y)
  ; ((x . y))
  (== y x) ; (unify y x ((x . y)))
  )

(define ext-s
  (lambda (u v s)
    (cons (cons u v) s)))  ; `((,u . ,v) . ,s)

(== `(5 . ,x)
    `(5 . ,y)) 

(define (unify u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)] ; left out the occur check 
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s)))]
      [else (and (eqv? u v) s)])))

;; (unify u v s) can return one of three values:
;; 
;; 1. #f  (represents failure of u and v to unify in s)
;; 2. s   (same s as was passed into unify -- no extension to substitution; u & v equal)
;; 3. s^  (s^ is a non-empty extension to s; u & v are not equal, but can be made equal)

(== 5 5) ; (2) succeed, no need to extend the substitution (throw away the == constraint)
(== 5 6) ; (1) fail
(== 5 x) ; (3) s^ = ((x . 5) . s); succeeds, need to add constraint that x is 5
(== `(5 . ,x) `(5 . ,y)) ; (3) s^ = ((x . y) . s)
;; succeeds, x and y equal, simplify constraint to (== x y)
(== `(5 . ,x) `(6 . ,y)) ; (1) fails

;; constraint store c, which include both an s and a disequality constraint store d
;; c = `(,s ,d)     d is a list of mini-substitutions
(=/= 5 5) ; (2) fail
(=/= 5 6) ; (1) succeeds, can never be violated; return the original c
(=/= 5 x) ; (3) succeeds, but x can not be instantiated to 5 later in the program
;; s^ = ((x . 5) . s)   prefix of s = ((x . 5)) <- mini substitution
;; the mini substitution is the normalized form of the disequality constraint,
;; and can be added to the 'd' part of the constraint store
(=/= `(5 . ,x) `(5 . ,y)) ; (3) succeed, but x and y can never be equal; (=/= x y)
;; s^ = ((x . y) . s)    prefix of s = ((x . y)) <- mini substitution
(=/= `(5 . ,x) `(6 . ,y)) ; (1) succeeds, can never be violated

(fresh (x y z)
  (=/= `(,x . ,y) `(,z . 5)))

;; think about (=/= `(,x . ,y) `(,z . 5)) in terms of (== `(,x . ,y)
;;                                                        `(,z . 5)),
;; which calls (unify `(,x . ,y) `(,z . 5) s)
;;
;; Unification succeeds, producing s^ = `((,x . ,z) (,y . 5) . ,s)
;; Prefix is the mini-substitution `((,x . ,z) (,y . 5)) <- this goes in 'd'

(fresh (x y z)
  (=/= `(,x . ,y) `(,z . 5))
  ;; d = `(((,x . ,z) (,y . 5)))      c = `(,s ,d)
  (== y 5))

  ;; c = `(((,y . 5) . ,s)  <- new s, s1
  ;;       (((,x . ,z) (,y . 5)))) <- disequality store (list of association lists)

;; How do we solve the disequality constraint `((,x . ,z)
;;                                              (,y . 5)) in s1?

;; Unify two terms in s1
;; (unify `(,x ,y) `(,z 5) `((,y . 5) . ,s))
;;
;; equivalent to
;;
;; (unify `(,x 5) `(,z 5) `((,y . 5) . ,s))
;;
;; equivalent to
;;
;; (unify x z `((,y . 5) . ,s))
;;
;; s^ = `((,x . ,z) (,y . 5) . ,s)
;;
;; prefix of s^ is `((,x . ,z))
;;
;; new c = `(((,y . 5) . ,s)
;;          (((,x . ,z)))) <- new d

(fresh (x y z)
  ;; d = ()                           c = `(,s ,d)
  (=/= `(,x . ,y) `(,z . 5))
  ;; d1 = `(((,x . ,z) (,y . 5)))     c = `(,s ,d1)
  (== y 5)
  ;; s1 = `((,y . 5) . ,s)
  ;; d2 = `(((,x . ,z)))
  (== x z)
  ;; s2 = `((,x . ,z) (,y . 5) . ,s)
  ;; (unify x z s2) => s2   violated disequality constraint ; fail!
  )


(fresh (x y)
  ;; c = `(,s ,d)
  (=/= x 5)
  ;; c1 = `(,s (((,x . 5)) . ,d))
  (=/= x 6)
  ;; c2 = `(,s (((,x . 6))
  ;;            ((,x . 5))
  ;;            . ,d))
  (=/= x y)
  ;; c3 = `(,s (((,x . y))
  ;;            ((,x . 6))
  ;;            ((,x . 5))
  ;;            . ,d))
  )

(fresh (x y)
;;  c = `(,s ,d)
  (=/= x y)
;;  c1 = `(,s (((,x . ,y)) . ,d))
  (== x 5)
;;  c2 = `(((,x . 5) . ,s)
;;         (((,y . 5)) . ,d)) 
  (== y 6)
;;  c3 = `(((,y . 6) (,x . 5) . ,s)
;;         ,d)
  )


;; subsumption example (violating one constraint necessarily violates
;; another constraint)
(fresh (x y z)
  ;; c = `(,s ,d)
  (=/= x 5)
  ;; c1 = `(,s (((,x . 5)) . ,d))
  (=/= `(,x . ,y)
       `(5 . ,z))
  ;; c2 = `(,s
  ;;         (((,x . 5) (,y . ,z))
  ;;          ((,x . 5)) <- mini substitution for (=/= x 5)
  ;;          . ,d))
  ;;
  ;; =/= constraint introduced by the second =/= call is redundant!
  ;; (subsumed by the first constraint)
  ;;
  ;; can throw away that constraint to get the simplified
  ;;
  ;;
  ;; c2 = `(,s
  ;;         (((,x . 5)) <- mini substitution for (=/= x 5)
  ;;          . ,d))
  
  )

  (=/= `(,x . ,y)
       `(5 . ,z))

;; equal?
;;
;; x = 5 and y = 6 and z = 6
;; then
;; `(,x . ,y) = `(5 . 6)
;; `(5 . ,z)  = `(5 . 6)

;; core.logic in Clojure uses != instead of =/=

;; Excellent survey paper on unification:
;;
;; Unification: A multidisciplinary survey
;; Kevin Knight
;; http://www.isi.edu/natural-language/people/unification-knight.pdf

;; Disunification papers:
;;
;; Equational Problems and Disunification (1989)
;; Hubert Comon , Pierre Lescanne
;; http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.139.4769
;;
;; Disunification: a Survey (1991)
;; Hubert Comon
;; http://citeseer.uark.edu:8080/citeseerx/viewdoc/summary?doi=10.1.1.48.8234
;;
;; Combining Unification- and Disunification Algorithms - Tractable and Intractable Instances (1996)
;; Klaus U. Schulz
;; http://citeseer.uark.edu:8080/citeseerx/viewdoc/summary?doi=10.1.1.49.7617

;; ()
(== `(5 ,x)
    `(,y 6))
;; ((y . 5) (x . 6))

(== `(,x) x)


((wyz . 8) ... (y . 6) (x . 5))

(fresh (x _)
  (== `(,x ,_) `(5 6)))

(== _ 5)
(== `(,x ,_) `(5 6))

((lambda (x)
   (fresh ()
     (== x 5)
     (== x 6)))
 _)

((x . 5) (_ . 6))


(fresh (major)
  (== `(byrd will computer-science senior)
      `(_ _ ,major _)))

(matche `(byrd will computer-science senior)
  [(_ _ ,major _)
   ])

;; Daniel's suggestion

(discard (_' _'' _''')
  (== `(byrd will cs senior) `(_' _'' major _''')))


;; Bones link, from Peter
;; http://www.call-with-current-continuation.org/bones/



;; Paper link from Daniel
;; "A Persistent Union-Find Data Structure" by Conchon et al. (2007)
;; https://www.lri.fr/~filliatr/puf/

;; Essentials of Logic Programming
;; Chris Hogger


;; Be careful when composing constraints!!
(fresh (x)
  (symbolo x)
  (numbero x) ; <- should fail!!!
  (f x) ; <- goal that diverges
  )

(fresh (x)
  (booleano x)
  (=/= #t x)
  (=/= #f x) ;; <- should fail!!!
  )

;;;

; unif/disunif

; CLP(tree)

; substitution

; variables

; goals, goal constructors, streams
