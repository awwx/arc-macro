(test (or) nil)

(let x 0
  (test (or (++ x)) 1)
  (test x 1))
