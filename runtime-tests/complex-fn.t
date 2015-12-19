(test (fn-complex-opt 'f 'list '(cdr g1))
  '((f (if (acons (cdr g1)) (car (cdr g1)) list))))


(test (cadar  '((a b c) d e)) 'b)
(test (caddar '((a b c) d e)) 'c)

(test (fn-complex-args 'xs '(car gs1))
  '((xs (car gs1))))

(test (fn-complex-args '(xs (o f list)) 'gs1)
  `((xs (,car gs1))
    (f  (if (acons (,cdr gs1))
             (car (,cdr gs1))
             list))))

(test (fn-complex-args? '())      nil)
(test (fn-complex-args? 'a)       nil)
(test (fn-complex-args? '(a))     nil)
(test (fn-complex-args? '(a . b)) nil)
(test (fn-complex-args? '((a)))   t)

; todo move?
(test (let fn + (fn 6 7)) 13)
