(test (single '())      nil)
(test (single '(a))     t)
(test (single '(a . b)) nil)
(test (single '(a b))   nil)
