(let a 10
  (++ a)
  (test a 11))

(let a "Hello"
  (++ a " Earthling")
  (test a "Hello Earthling"))

(let a '(10 20 30 40)
  (++ (car a))
  (test a '(11 20 30 40)))

(let a '(10 20 30 40)
  (++ (car (cdr a)))
  (test a '(10 21 30 40)))

(let a '(10 20 30 40)
  (++ (cadr a))
  (test a '(10 21 30 40)))

(let a '(10 20 30 40)
  (++ (a 0))
  (test a '(11 20 30 40)))

(let a '(10 20 30 40)
  (++ (a 2))
  (test a '(10 20 31 40)))

(let a (obj x 10)
  (++ a!x)
  (test a (obj x 11)))

(let a (obj x '(10 20 30))
  (++ (car a!x))
  (test a (obj x '(11 20 30))))
