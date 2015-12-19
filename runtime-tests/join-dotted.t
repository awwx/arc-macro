(test (join nil 'a)    'a)
(test (join '(a) 'b)   '(a . b))
(test (join '(a b) 'c) '(a b . c))
