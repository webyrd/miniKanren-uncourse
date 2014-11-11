Vicare Scheme version 0.1d2+, 64-bit (revision 08bd828acfa9382324150b41f4e86c540c10a886, build 2013-08-27)
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors

> (load "mk.scm")
> ==
#<procedure == [char 17082 of mk.scm]>
> (equal? 5 5)
#t
> (equal? 5 6)
#f
> (equal? '(3 4 5) '(3 4 5))
#t
> (equal? '(3 ((((4)))) 5) '(3 ((((4)))) 5))
#t
> (equal? '(3 ((((4)))) 5) '(3 5 ((((4))))))
#f
> (== '(3 ((((4)))) 5) '(3 5 ((((4))))))
#<procedure>
> (run 1 (q)
    (== '(3 ((((4)))) 5) '(3 5 ((((4)))))))
()
> (run 1 (q)
    (== '(3 ((((4)))) 5) '(3 ((((4)))) 5)))
(_.0)
> (run 1 (q)
    (== 5 6))
()
> (run 1 (q)
    (== 5 5))
(_.0)
> (run 1 (q)
    (== 5 q))
(5)
> (run 1 (q)
    (== q q))
(_.0)
> (run 1 (q)
    (== (list 3 4) (list 3 4)))
(_.0)
> (run 1 (q)
    (== (list q 4) (list 3 4)))
(3)
> (run 1 (q)
    (== (list q 4) (list q 4)))
(_.0)
> (run 1 (q)
    (== (list q 4)
        (list 4 q)))
(4)
> (run 1 (q)
    (== (list q 4)
        (list 3 q)))
()
> (run 1 (q)
    (== q (list 3)))
((3))
> (run 1 (q)
    (== q (list q)))
()
> (run 1 (q)
    (== (list q 4)
        (list 3 q)))
()
> (run 1 (x y)
    (== (list x 4)
        (list 3 y)))
((3 4))
> (run 1 (q)
    (fresh (x y)
      (== (list x 4)
          (list 3 y))))
(_.0)
> (run 1 q
    (fresh (x y)
      (== (list x 4)
          (list 3 y))
      (== (list x y) q)))
((3 4))
> (run 1 (x y)
    (== (list x 4)
        (list 3 y)))
((3 4))
> (lambda (x) x)
#<procedure>
> ((lambda (x) x) (+ 3 4))
7
> ((lambda (y) y) (+ 3 4))
7
> ((lambda (lambda) lambda) (+ 3 4))
7
> '(lambda (x) x)
(lambda (x) x)
> '(lambda (y) y)
(lambda (y) y)
> (equal?  '(lambda (y) y))
#f
> (equal? '(lambda (x) x) '(lambda (x) x))
#t
> (lambda (x) (lambda (y) (x y)))
#<procedure>
> (run 1 (q))
Unhandled exception
 Condition components:
   1. &message: "invalid syntax"
   2. &syntax:
       form: (run 1 (q))
       subform: #f
   3. &trace: #<syntax (run 1 (q))>
> (run 1 (q)
    (== '(lambda (x) x) '(lambda (x) x)))
(_.0)
> (run 1 (q)
    (== '(lambda (x) x) '(lambda (y) y)))
()
> (run 1 (x y)
    (== `(lambda (,x) ,x) `(lambda (,y) ,y)))
((_.0 _.0))
> (run 1 (x y)
    (== (list 'lambda (list x) x)
        `(lambda (,y) ,y)))
((_.0 _.0))
> (run 1 (x y)
    (fresh (t1 t2)
      (== `(lambda (,x) ,x) t1)
      (== `(lambda (,y) ,y) t2)))
((_.0 _.1))
> (run 1 (x y)
    (fresh (t1 t2)
      (== t1 t2)
      (== `(lambda (,x) ,x) t1)
      (== `(lambda (,y) ,y) t2)))
((_.0 _.0))
> (run 1 (x y)
    (== `(lambda (,x) ,x) `(lambda (,y) ,y)))
((_.0 _.0))
> (run 1 (x y)
    (== `(lambda (,x) ,x) `(lambda (,x) ,y)))
((_.0 _.0))
> (run 1 [x y]
    (== `(fn [~x] ~x) `(fn [~y] ~y)))
()
> (ru 1 (y)
      (== (list y 3)
          (list 4 y)))
Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (y)
> (run 1 (y)
    (== (list y 3)
        (list 4 y)))
()
> 
