(test ((fn ((o a 3)) a))
  3)

(test ((fn ((a b c))
         (list a b c))
       '(1 2 3))
  '(1 2 3))

(test ((fn ((a (b c) d))
         (list a b c d))
       '(1 (2 3) 4))
  '(1 2 3 4))
