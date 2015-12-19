(testx (has-ssyntax-char _)
  "abc"    nil
  "ab:cd"  t)

(testx (is-ssyntax _)
  'abc    nil
  ':      t
  'ac.cd  t
  'ab:cd  t)

(testx (insym #\: _)
  'a:b  t
  'abc  nil)

(test (ssyntax-tokens #\: (coerce (coerce 'abc:def 'string) 'cons) nil nil nil)
  '((#\a #\b #\c) (#\d #\e #\f)))

(testx (ssyntax-tokens [in _ #\. #\!] (coerce (coerce _ 'string) 'cons) nil nil t)
  'a.b!c  '((#\a) #\. (#\b) #\! (#\c))
  'a!b!!  '((#\a) #\! (#\b) #\! #\!))

(testx (ssyntax-expand-compose _)
  'a:b    '(compose a b)
  'a:b:c  '(compose a b c)
  '~      'no
  '~a     '(complement a)
  '~a:b   '(compose (complement a) b)
  'a:~b   '(compose a (complement b))
  '~a:~b  '(compose (complement a) (complement b)))


(test (ssyntax-build-sexpr '((#\b) #\! (#\a)) 'a!b)
  '(a (quote b)))

(test (ssyntax-build-sexpr '((#\b) #\. (#\a)) 'a.b)
  '(a b))

(test (ssyntax-build-sexpr '((#\c) #\. (#\b) #\. (#\a)) 'a.b.c)
  '((a b) c))

(testx (ssyntax-expand-sexpr _)
  'a.b    '(a b)
  'a!b    '(a (quote b))
  'a.b.c  '((a b) c)
  'a!b.c  '((a (quote b)) c))

(testx (ssyntax-expand-and _)
  'a&b   '(andf a b)
  'a&b&c '(andf a b c))
