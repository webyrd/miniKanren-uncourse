;; miniKanren Hangout #11:
;;
;; Cool stuff from Daniel, orchid-hybrid, and David!


Daniel showed off his Elementary Cellular Automata implementation in core.logic.  (Is there a link to the code?)



orchid-hybrid's R7RS microKanren implementation, complete with record-based constraint store and disequality constraints:

https://github.com/orchid-hybrid/microKanren-sagittarius

Sample query:

(run* (lambda (q) (fresh (x y) (=/= q (list x y)))))
=>
((.0 (and (or (=/= _.0 (.1 _.2))))))




David's Veneer editor.  Edit and run miniKanren in the browser!

http://tca.github.io/veneer/editor.html

https://github.com/tca/veneer
