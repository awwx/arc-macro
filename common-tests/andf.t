(with (1+ [+ _ 1]
       even [is (mod _ 2) 0])
  (testx ((andf even 1+) _)
    1  nil
    2  3
    3  nil))
