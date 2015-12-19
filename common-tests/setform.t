(mac testf (orig mod val expected)
  `(let x ',orig
     (= ,mod ,val)
     (test x ',expected)))

(testf (a b)         (car x)                   3  (3 b))
(testf ((a b) (c d)) (car (car x))             4  ((4 b) (c d)))
(testf ((a b) (c d)) (caar x)                  5  ((5 b) (c d)))
(testf ((a b) (c d)) (car:car x)               6  ((6 b) (c d)))
(testf ((a b) (c d)) (car (cdr (car (cdr x)))) 7  ((a b) (c 7)))
(testf (a b c d e)   (x 2)                     8  (a b 8 d e))

(let g (table)
  (= (g 'foo) 8)
  (test (g 'foo) 8))

; todo

; (let x '(1 2 3)
;   (++ (car x))
;   (test x '(2 2 3)))

; (let x (obj a 1 b 2 c 3)
;   (++ x!c)
;   (test x!c 4))
