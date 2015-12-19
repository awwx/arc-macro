(testx (pair _)
  '()             '()
  '(a)            '((a))
  '(a b c d e)    '((a b) (c d) (e))
  '(a b c d e f)  '((a b) (c d) (e f)))

(testx (pair _ cons)
  '(a b c d)      '((a . b) (c . d)))

(testx (pair _ +)
  '(1 2 3 4)      '(3 7))
