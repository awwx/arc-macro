(def ssyntax-char (c)
  (in c #\: #\~ #\& #\. #\!))

(def has-ssyntax-char (s)
  (some ssyntax-char s))

(def is-ssyntax (x)
  (and (isa x 'sym)
       (no (in x '+ '++ '_))
       (has-ssyntax-char (coerce x 'string))))

(def insym (char sym)
  (some char (coerce sym 'string)))

(def ssyntax-tokens (test source token acc keepsep)
  (if (no source)
       (rev (if (acons token)
                 (cons (rev token) acc)
                 acc))
      ((testify test) (car source))
       (ssyntax-tokens test
                       (cdr source)
                       nil
                       (let rec (if (no token)
                                    acc
                                    (cons (rev token) acc))
                         (if keepsep
                             (cons (car source) rec)
                             rec))
                       keepsep)
       (ssyntax-tokens test
                       (cdr source)
                       (cons (car source) token)
                       acc
                       keepsep)))

(def chars->value (cs)
  (coerce (coerce cs 'string) 'sym))

(def symbol->chars (sym)
  (coerce (coerce sym 'string) 'cons))

(def ssyntax-expand-compose (sym)
  (let elts (map (fn (tok)
                   (if (caris tok #\~)
                        (if (no (cdr tok))
                             'no
                             `(complement ,(chars->value (cdr tok))))
                        (chars->value tok)))
                 (ssyntax-tokens #\: (symbol->chars sym) nil nil nil))
    (if (no (cdr elts))
         (car elts)
         (cons 'compose elts))))

(def ssyntax-build-sexpr (toks orig)
  (if (no toks)
       'get
      (no (cdr toks))
       (chars->value (car toks))
       (list (ssyntax-build-sexpr (cddr toks) orig)
             (if (is (cadr toks) #\!)
                  (list 'quote (chars->value (car toks)))
                  (if (in (car toks) #\. #\!)
                       (err "Bad ssyntax" orig)
                       (chars->value (car toks)))))))

(def ssyntax-expand-sexpr (sym)
  (ssyntax-build-sexpr
   (rev (ssyntax-tokens [in _ #\. #\!] (symbol->chars sym) nil nil t))
   sym))

(def ssyntax-expand-and sym
  (let elts (map chars->value
                 (ssyntax-tokens #\&  (symbol->chars sym) nil nil nil))
    (if (no (cdr elts))
         (car elts)
         (cons 'andf elts))))

(def expand-ssyntax (sym)
  ((if (or (insym #\: sym) (insym #\~ sym))
        ssyntax-expand-compose
       (or (insym #\. sym) (insym #\! sym))
        ssyntax-expand-sexpr
       (insym #\& sym)
        ssyntax-expand-and
        (err "Unknown ssyntax" sym))
   sym))

(def ssexpand (x)
  (if (isa x 'sym)
       (expand-ssyntax x)
       x))
