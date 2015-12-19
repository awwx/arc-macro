; (mac named-fn (name parms . body)
;   `(,namefn ',name (,fn ,parms ,@body)))

(assign named-fn
  (annotate 'mac
    (fn (name parms . body)
      (cons namefn
        (cons (cons quote (cons name nil))
          (cons (cons fn (cons parms body)) nil))))))


; (mac def (name parms . body)
;   `(,assign ,name (,named-fn ,name ,parms ,@body)))

(assign def
  (annotate 'mac
    (fn (name parms . body)
      (cons assign
        (cons name
          (cons (cons named-fn (cons name (cons parms body)))
            nil))))))
