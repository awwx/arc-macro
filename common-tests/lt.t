(test (< 2 3)     t)
(test (< 3 2)     nil)
(test (< 2 3 4)   t)
(test (< 2 4 7 9) t)
(test (< 4 2 7 9) nil)
(test (< 2 4 9 7) nil)
(test (< 2 7 4 9) nil)
