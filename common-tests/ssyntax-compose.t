(test (~ nil) t)
(test (~ t)   nil)

(test (~acons 123) t)

(with (5+ [+ _ 5]
       7+ [+ _ 7])
  (test (5+:7+    6)     18)
  (test (5+:7+:5+ 6)     23)
  (test (5+:7+:*  4 6 7) 180))

(testx (~acons:car _)
  '(123)    t
  '((123))  nil)
