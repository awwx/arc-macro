; (mac if args
;   (if (no args)
;        nil
;       (no cdr args)
;        (car args)
;        `(,if ,(car args)
;               ,(cadr args)
;               (,if ,@(cddr args)))))

(assign if
  (annotate 'mac
    (fn args
      ($if--xVrP8JItk2Ot (no args)
        nil
        ($if--xVrP8JItk2Ot (no (cdr args))
          (car args)
          (list '$if--xVrP8JItk2Ot
                (car args)
                (cadr args)
                (cons if (cddr args))))))))
