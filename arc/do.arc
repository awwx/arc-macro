; (mac do args
;   `((,fn () ,@args)))

(assign do
  (annotate 'mac
    (fn args
      (cons (cons fn (cons '() args))
            '()))))
