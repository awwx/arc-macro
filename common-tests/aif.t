(test (aif 1 it)               1)
(test (aif nil it 3)           3)
(test (aif 1 it 3)             1)
(test (aif 1 it 2 it)          1)
(test (aif nil it 2 it)        2)
(test (aif nil it nil it 4 it) 4)

(let x 0
  (test (aif (do (assign x (+ x 1)) x) it) 1))
