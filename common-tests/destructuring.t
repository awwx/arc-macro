(let f (fn ((a b c))
         (list a b c))
  (test (f '(1 2 3)) '(1 2 3))
  (test (f '(1 2))   '(1 2 nil))
  (test (f '())      '(nil nil nil)))

(let f (fn ((a (b c) d))
         (list a b c d))
  (test (f '(1 (2 3) 4)) '(1 2 3 4)))
