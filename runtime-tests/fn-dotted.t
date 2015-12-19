(test ((fn rest rest)) nil)

(test ((fn rest             (list rest))         1 2 3 4) '((1 2 3 4)))
(test ((fn (a . rest)       (list a rest))       1 2 3 4) '(1 (2 3 4)))
(test ((fn (a b . rest)     (list a b rest))     1 2 3 4) '(1 2 (3 4)))
(test ((fn (a b c . rest)   (list a b c rest))   1 2 3 4) '(1 2 3 (4)))
(test ((fn (a b c d . rest) (list a b c d rest)) 1 2 3 4) '(1 2 3 4 ()))
