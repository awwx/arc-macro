(test (set-equal '() '()) t)
(test (set-equal '(a) '(a)) t)
(test (set-equal '(a) '(b)) nil)
(test (set-equal '(a b) '(a b)) t)
(test (set-equal '(b a) '(b a)) t)

(test (table-equal (obj) (obj)) t)
(test (table-equal (obj a 1) (obj a 1)) t)
(test (table-equal (obj a 1 b 2) (obj a 1 b 2)) t)

(test (iso 'a 'a) t)
(test (iso '(a b) '(a b)) t)
(test (iso (obj a 1) (obj a 1)) t)
