(test (literal 3)              t)
(test (literal car)            t)
(test (literal "abc")          t)
(test (literal 'foo)           nil)
(test (literal 'nil)           t)
(test (literal 't)             t)
(test (literal '(4 5))         nil)
(test (literal '(quote 3))     t)
(test (literal (list quote 3)) t)