(def amacro (x)
  (and (isa x 'mac) x))

(def macro (x globals)
  (or (amacro x)
      (and (isa x 'sym)
           (amacro (globals x)))))

(def is-lexical (v env)
  (and (isa v 'sym) (mem v env)))

(def macro-expand (e globals (o env nil))
  (aif (is e 'nil)
        `($quote--xVrP8JItk2Ot nil)
       (and (isa e 'sym) (is-ssyntax e))
        (macro-expand (expand-ssyntax e) globals env)
       (isa e 'sym)
        (macro-expand-var e globals env)
       (caris e '$assign--xVrP8JItk2Ot)
        (macro-expand-assign e globals env)
       (caris e '$fn--xVrP8JItk2Ot)
        (macro-expand-fn e globals env)
       (caris e '$if--xVrP8JItk2Ot)
        (macro-expand-if e globals env)
       (caris e '$quote--xVrP8JItk2Ot)
        e
       (and (acons e)
            (~is-lexical (car e) env)
            (macro (car e) globals))
        (expand-macro it (cdr e) globals env)
       (acons e)
        (macro-expand-call e globals env)
        e))

(def macro-expand-call (e globals env)
  (macro-expand-each e globals env))

(def macro-expand-each (es globals env)
  (map [macro-expand _ globals env] es))

(def macro-expand-var (var globals env)
  (if (is-lexical var env)
       var
       (macro-expand-global-var var globals env)))

(def macro-expand-global-var (var globals env)
  (if (is var 'globals)
       globals
       (let global-macro (globals 'global)
         (unless global-macro
           (err "no `global` macro defined for global variable reference:" var))
         (macro-expand `(,global-macro ,var) globals env))))

(def expand-macro (m args globals env)
  (macro-expand (apply (rep m) args) globals env))

(def macro-expand-assign ((_assign var value) globals env)
  (if (is-lexical var env)
       `($assign--xVrP8JItk2Ot ,var ,(macro-expand value globals env))
       (let set-global (globals 'set-global)
         (unless set-global
           (err "no `set-global` macro defined for setting global variable:" var))
         (macro-expand `(,set-global ,var ,value) globals env))))

(def arglist (args)
  (if (no args)
       nil
      (isa args 'sym)
       (list args)
      (and (cdr args) (isa (cdr args) 'sym))
       (list (car args) (cdr args))
       (cons (car args) (arglist (cdr args)))))

(def macro-expand-fn ((_fn args . body) globals env)
  `($fn--xVrP8JItk2Ot ,args
     ,@(macro-expand-each body globals (join (arglist args) env))))

(def macro-expand-if ((_if . body) globals env)
  `($if--xVrP8JItk2Ot ,@(macro-expand-each body globals env)))

(def unprimitive (x)
  (if (no (acons x))
       x
      (caris x '$quote--xVrP8JItk2Ot)
       `(quote ,(cadr x))
      (caris x '$assign--xVrP8JItk2Ot)
       `(assign ,(x 1) ,(unprimitive (x 2)))
      (caris x '$fn--xVrP8JItk2Ot)
       `(fn ,(cadr x) ,@(map unprimitive (cddr x)))
      (caris x '$if--xVrP8JItk2Ot)
        `(if ,@(map unprimitive (cdr x)))
        (map unprimitive x)))
