(test (+)
  0)

(test (+ 1 2 3 4)
  10)

(test (+ '(a b) '(c d e))
  '(a b c d e))

(test (+ "abc" 123 'xyz)
  "abc123xyz")

(test (+ #\a #\b #\c)
  "abc")
