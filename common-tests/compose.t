(test ((compose)             '(a b c)) '(a b c))
(test ((compose car)         '(a b c)) 'a)
(test ((compose car cdr)     '(a b c)) 'b)
(test ((compose car cdr cdr) '(a b c)) 'c)

(with (5+ [+ _ 5]
       7+ [+ _ 7])
  (test ((compose 5+ 7+) 6)       18)
  (test ((compose 5+ 7+ 5+) 6)    23)
  (test ((compose 5+ 7+ *) 4 6 7) 180))
