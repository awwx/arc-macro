(test (with (i 5 x 0)
        (while (isnt (-- i) 0)
          (assign x (+ x i)))
        x)
  10)
