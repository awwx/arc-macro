(test (withs (a 1 b (+ a 1))
        (list a b))
  '(1 2))

(test (withs (a 1 b (+ a 1) c (+ a b))
        (list a b c))
  '(1 2 3))
