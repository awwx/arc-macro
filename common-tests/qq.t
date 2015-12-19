; These are quasiquote expressions supported in Arc 3.1

(test `(a (b . c) d)
  '(a (b . c) d))

(test `(a ,(+ 2 3) b)
  '(a 5 b))

(test `(a ,@(list 'b 'c) d)
  '(a b c d))
