; Unlike the "dotted" function in Arc 3.1 arc.arc, return t for a
; single symbol being a dotted param; for example,
; "args" in (fn args ...)
;
; todo: This might the same as ~proper from qq.arc?

(def dotted-parm (x)
  (if (no x)
       nil
      (acons x)
       (dotted-parm (cdr x))
       t))

(test (dotted-parm nil)      nil)
(test (dotted-parm 'a)       t)
(test (dotted-parm '(a))     nil)
(test (dotted-parm '(a . b)) t)
(test (dotted-parm '(a b))   nil)

(def undotted-prefix (x)
  (if (acons x)
       (if (acons (cdr x))
            (cons (car x) (undotted-prefix (cdr x)))
            (list (car x)))
       nil))

(test (undotted-prefix 'a)           '())
(test (undotted-prefix '(a . b))     '(a))
(test (undotted-prefix '(a b . c))   '(a b))
(test (undotted-prefix '(a b c . d)) '(a b c))

(def end-dot (x)
  (if (acons x)
       (if (acons (cdr x))
            (end-dot (cdr x))
            (cdr x))
       x))

(test (end-dot 'a)           'a)
(test (end-dot '(a . b))     'b)
(test (end-dot '(a b . c))   'c)
(test (end-dot '(a b c . d)) 'd)

(def join-dotted args
  (if (no args)
       nil
      (let a (car args)
        (if (no (cdr args))
             a
            (no a)
             (apply join-dotted (cdr args))
             (cons (car a) (apply join-dotted (cdr a) (cdr args)))))))

(test (join-dotted '(a b) 'c) '(a b . c))

(with ($assign '$assign--xVrP8JItk2Ot
       $fn     '$fn--xVrP8JItk2Ot
       $if     '$if--xVrP8JItk2Ot
       $quote  '$quote--xVrP8JItk2Ot)

  (def ac-racket (s)
    (if (is s nil)
         `(quote (arc--XwSDNhnd2nCJ nil))
        (caris s $assign)
         (ac-racket-assign (s 1) (s 2))
        (caris s $fn)
         (ac-racket-fn (cadr s) (cddr s))
        (caris s $if)
          (let (a b c) (cdr s)
            `(if (eqv? ,(ac-racket a) (quote (arc--XwSDNhnd2nCJ nil)))
                  ,(ac-racket c)
                  ,(ac-racket b)))
        (caris s $quote)
         `(quote (arc--XwSDNhnd2nCJ ,(cadr s)))
        (acons s)
         (ac-racket-call s)
         s))

  (def ac-racket-assign (var value)
    (unless (isa var 'sym)
      (err "First arg to assign must be a symbol"))
    (w/uniq v
      `(let ((,v ,(ac-racket value)))
         (set! ,var ,v)
         ,v)))

  (def ac-racket-call (args)
    `(,ar-funcall
      ,@(map ac-racket args)))

  (def ac-racket-fn (parms body)
    (if (dotted-parm parms)
         (ac-racket-dotted-fn parms body)
         `(lambda ,parms ,@(map ac-racket body))))

  (def ac-racket-dotted-fn (parms body)
    (w/uniq rest
      `(lambda ,(join-dotted (undotted-prefix parms) rest)
         (let ((,(end-dot parms)
                (,ar-toarclist ,rest)))
           ,@(map ac-racket body))))))
