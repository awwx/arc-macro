(with (x (obj a 1 b 2 c 3) i 'b)
  (test x!a 1)
  (test x.i 2))

(let x (table)
  (= x!a 24)
  (test (x 'a) 24))

(= test-table (table))

(= test-table!foo 42)

(test (test-table 'foo) 42)

(wipe test-table)
