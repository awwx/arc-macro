(def join args
  (if (no args)
       nil
      (let a (car args)
        (if (no (cdr args))
             a
            (no a)
             (apply join (cdr args))
             (cons (car a) (apply join (cdr a) (cdr args)))))))
