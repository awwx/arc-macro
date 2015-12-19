(test (car nil) nil)

(testx (car _)
  nil      nil
  '(a b c) 'a
  3        '(err "Can't take car of 3"))

(testx (cdr _)
  nil      nil
  '(a . b) 'b
  '(a b)   '(b)
  3        '(err "Can't take cdr of 3"))
