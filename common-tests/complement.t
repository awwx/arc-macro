(test ((complement car) nil)  t)
(test ((complement car) '(a)) nil)
(test (< 3 7) t)
(test ((complement <) 3 7) nil)
