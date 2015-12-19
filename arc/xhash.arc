(def xhash (x)
  (if (isa x 'table)
       '<<table>>
      (acons x)
       (cons (xhash (car x)) (xhash (cdr x)))
       x))
