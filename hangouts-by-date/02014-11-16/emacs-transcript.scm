Vicare Scheme version 0.1d2+, 64-bit (revision 08bd828acfa9382324150b41f4e86c540c10a886, build 2013-08-27)
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors

> (load "mk.scm")
> (run 1 (q) (== 5 5))
(_.0)
> (run 1 (q) (== 5 6))
()
> (run 2 (q) (== 5 6))
()
> (run 2 (q) (== 5 5))
(_.0)
> (run* (q) (== 5 5))
(_.0)
> (run* (q) (== q 5))
(5)
> (run* (q)
    (conde
      [(== q 5)]
      [(== q 6)]))
(5 6)
> (run 1 (q)
    (conde
      [(== q 5)]
      [(== q 6)]))
(5)
> (run 2 (q)
    (conde
      [(== q 5)]
      [(== q 6)]))
(5 6)
> (run 3 (q)
    (conde
      [(== q 5)]
      [(== q 6)]))
(5 6)
> (run 10000 (q)
    (conde
      [(== q 5)]
      [(== q 6)]))
(5 6)
> (run* (q)
    (conde
      [(== q 5)]
      [(== q 6)]))
(5 6)
> (run* (q)
    (conde
      [(== q 5)]
      [(== q 6)]
      [(== q 7)]))
(5 6 7)
> (run* (q)
    (conde
      [(== q 5)]
      [(== q 6)]
      [(== q 7)]))
(5 6 7)
> (list (run* (q) (== q 5)))
((5))
> (list (run* (q) (== q 5)) (run* (q) (== q 6)) (run* (q) (== q 7)))
((5) (6) (7))
> (apply append (list (run* (q) (== q 5))
                      (run* (q) (== q 6))
                      (run* (q) (== q 7))))
(5 6 7)
> (run* (q)
    (conde
      [(== q 5)]
      [(== q 6)]
      [(== q 7)]))
(5 6 7)
> (run* (q)
    (conde
      [(== q 5) (== 5 q)]
      [(== q 6)]
      [(== q 7)]))
(5 6 7)
> (run* (q)
    (conde
      [(== q 5) (== 4 q)] ; fail
      [(== q 6)]          ; succeed  q = 6
      [(== q 7)]))        ; succeed  q = 7
(6 7)
> (run* (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 6)]
      [(== q 7)]))
(6 7)
> (run* (q)
    (conde
      [(== q 7)]
      [(== q 6)]
      [(== q 5) (== 4 q)]))
(7 6)
> (run* (q)
    (conde
      [(== q 6)]
      [(== q 7)]
      [(== q 5) (== 4 q)]))
(6 7)
> (run 1 (q)
    (conde
      [(== q 6)]
      [(== q 7)]
      [(== q 5) (== 4 q)]))
(6)
> (run 1 (q)
    (conde
      [(== q 7)]
      [(== q 5) (== 4 q)]
      [(== q 6)]))
(7)
> (run* (q)
    (conde
      [(== q 7)]
      [(== q 5) (== 4 q)]
      [(== q 6)]))
(7 6)
> (run* (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 7)]
      [(== q 6)]))
(7 6)
> (run* (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 7)]
      [(== q 6)]))
(7 6)
> (run 1 (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 7)]
      [(== q 6)]))
(7)
> (run 2 (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 7)]
      [(== q 6)]))
(7 6)
> (run 3 (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 7)]
      [(== q 6)]))
(7 6)
> (run 1 (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 7)]
      [(== q 6)]))
(7)
> (run 2 (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 7)]
      [(== q 6)]))
(7 6)
> (run 2 (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 6)]
      [(== q 7)]))
(6 7)
> (run 1 (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 6)]
      [(== q 7)]))
(6)
> (run 1 (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 7)]
      [(== q 6)]))
(7)
> (run 1 (q)
    (conde
      [(== q 5) (== 4 q)]
      [(== q 7)]
      [(== q 6)]))
(7)
> (run 1 (q)
    (fresh (x y)
      (== (list x 5 y) q)))
((_.0 5 _.1)) ; reified answers
> 5
5
> (run 1 (q)
    (fresh (x y)
      (== (list x 5 y) q)))
((_.0 5 _.1))
> (run 1 (q x y)
    (== (list x 5 y) q))
(((_.0 5 _.1) _.0 _.1))
> (run 1 (q)
    (fresh (x y)
      (== (list x 5 y) q)))
((_.0 5 _.1))
> (run 1 (q)
    (fresh (x y)
      (== (list x 5 y) q)
      (== 7 y)))
((_.0 5 7))
> (run 1 (q)
    (fresh (x y)
      (== (list x 5 y) q)
      (== 7 y)))
((_.0 5 7))
> (run 1 (q)
    (fresh ()
      (== (list 4 5 6) q)
      (== 7 y)))
Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (y)
> (run 1 (q)
    (fresh ()
      (== (list 4 5 6) q)))
((4 5 6))
> (run 1 (q)
    (fresh ()
      (== (list 4 5 6) q)
      (== 7 7)))
((4 5 6))
> (run 1 (q)
    (== (list 4 5 6) q)
    (== 7 7))
((4 5 6))
> (run 1 (???)
    (fresh (q)
      (== ??? q)
      (== (list 4 5 6) q)
      (== 7 7)))
((4 5 6))
> (run 1 (???)
    (fresh (q)
      (== ??? q)
      (== (list 4 5 6) q)
      (== 7 7)))
((4 5 6))
>
Core miniKanren

conjunction  (and)           fresh/run/within a single conde clause
disjunction  (or)            conde
equality     (==)            ==
introducing fresh variable   fresh
--------------------------
interface/collecting answers   (run)

host language:
values (numbers, boolean constants, symbols, lists)
define, lambda, macros, recursion
Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (Core)
> Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (conjunction)
> Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (disjunction)
> Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (equality)
> Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (introducing)
> Unhandled exception
 Condition components:
   1. &lexical
   2. &message: "invalid numeric sequence"
   3. &irritants: ("--")
   4. &source-position:
       file-name: *stdin*
       character: 3488
> Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (interface/collecting)
> Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (host)
> #<procedure values>
> Unhandled exception
 Condition components:
   1. &who: unquote
   2. &message: "incorrect usage of auxiliary keyword"
   3. &syntax:
       form: ,boolean
       subform: #f
   4. &trace: #<syntax ,boolean>
> Unhandled exception
 Condition components:
   1. &who: define
   2. &message: "invalid expression"
   3. &syntax:
       form: define
       subform: #f
   4. &trace: #<syntax define>
> (run 1 (q) (== 5 5))
(_.0)
> (run 1 (q) (== '_.0 q))
(_.0)
> (run 1 (q) (== '_.1 q))
(_.1)
> (run 1 (q) (== '_.999 q))
(_.999)
> (run 1 (q) (== '_.0 q))
(_.0)
> (run 1 (q) (== '_.0 q) (== 5 q))
()
> (run 1 (q) (== '_.0 q))
(_.0)
> (run* (q) (symbolo q))
((_.0 (sym _.0)))
> (run* (q) (numbero q))
((_.0 (num _.0)))
> (run* (q) (numbero q) (== 'foo q))
()
> (run* (q) (numbero q) (== 5 q))
(5)
> 