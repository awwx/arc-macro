; Demo Arc runtime using the precompiled Arc compiler.

#lang racket

(display "ar.rkt\n")

(provide (all-defined-out))

(define-namespace-anchor anchor)

(define namespace (namespace-anchor->namespace anchor))

(define-syntax p
  (syntax-rules ()
    ((_ x)
     (begin (write 'x)
            (display ": ")
            (let ((r x))
              (write r)
              (newline)
              r)))))

(define-syntax test
  (syntax-rules ()
    ((_ expr expected)
     (let ((actual expr))
       (if (equal? actual expected)
            (begin (display "OK ")
                   (write 'expr)
                   (display " => ")
                   (write actual)
                   (newline))

            (begin (display "FAIL ")
                   (write 'expr)
                   (display " => ")
                   (write actual)
                   (newline)
                   (display "  not ")
                   (write expected)
                   (newline)
                   (exit 1)))))))

(define (read-square-brackets ch port src line col pos)
  `(square-bracket ,@(read/recursive port #\[ #f)))

(define bracket-readtable
  (make-readtable #f #\[ 'terminating-macro read-square-brackets))

(current-readtable bracket-readtable)

(define (table)
  (make-hasheqv))

(define (arc-car x)
  (cond ((eq? x 'nil)
         'nil)
        ((mpair? x)
         (mcar x))
        (else
         (err "Can't take car of" x))))

(define (arc-cdr x)
  (cond ((eq? x 'nil)
         'nil)
        ((mpair? x)
         (mcdr x))
        (else
         (err "Can't take cdr of" x))))

(define (arc-cons a b)
  (mcons a b))

(define (tnil x)
  (if x 't 'nil))

(define (ar-is2 a b)
  (tnil (or (eqv? a b)
            (and (string? a) (string? b) (string=? a b)))))

(define (toarc x)
  (cond ((pair? x)
         (mcons (toarc (car x)) (toarc (cdr x))))
        ((eq? x '())
         'nil)
        (else x)))

(test (toarc 3)      3)
(test (toarc '())    'nil)
(test (toarc '(a b)) (mcons 'a (mcons 'b 'nil)))

(define (arc-read in . rest)
  (let ((eof (if (pair? rest) (car rest) 'nil))
        (port (if (string? in) (open-input-string in) in)))
    (let ((x (read port)))
      (if (eof-object? x)
           eof
           (toarc x)))))

(test (arc-read "(a b c)") (toarc '(a b c)))
(test (arc-read "[a b c]") (toarc '(square-bracket a b c)))

(define (arclist x)
  (if (eq? x '())
       'nil
       (mcons (car x) (arclist (cdr x)))))

(define (alist . args)
  (arclist args))

(define (racketlist x)
  (cond ((eq? x 'nil)
         '())
        ((mpair? x)
         (cons (mcar x) (racketlist (mcdr x))))
        (else
         x)))

(define (denil x)
  (cond ((mpair? x)
         (cons (denil-car (mcar x))
               (denil-cdr (mcdr x))))
        (else
         x)))

(define (denil-car x)
  (if (eq? x 'nil)
       'nil
       (denil x)))

(define (denil-cdr x)
  (if (eq? x 'nil)
       '()
       (denil x)))

(define (fromarc x)
  (cond ((eq? x 'nil)
         '())
        ((mpair? x)
         (cons (fromarc (mcar x))
               (fromarc (mcdr x))))
        (else
         x)))

(define (toracket x)
  (cond ((eq? x 'nil)
         '())
        ((and (mpair? x) (eq? (mcar x) 'arc--XwSDNhnd2nCJ))
         (mcadr x))
        ((mpair? x)
         (cons (toracket (mcar x))
               (toracket (mcdr x))))
        (else
         x)))

(define (ar-assert x)
  (if (eq? x 'nil)
       (begin (display "assertion failed\n")
              (exit 1))
       (begin (display "ok ")
              (write x)
              (newline))))

(define (ar-+ a b)
  (+ a b))

(define (exint? x) (and (integer? x) (exact? x)))

(define (ar-type x)
  (cond ((tagged? x)        (tagged-type x))
        ((mpair? x)         'cons)
        ((symbol? x)        'sym)
        ((procedure? x)     'fn)
        ((char? x)          'char)
        ((string? x)        'string)
        ((exint? x)         'int)
        ((number? x)        'num)
        ((hash? x)          'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((thread? x)        'thread)
        (else               (error "type: unknown type" x))))

(define err error)

(define (iround x) (inexact->exact (round x)))

(define (ar-coerce x type . args)
  (cond
    ((tagged? x) (error "Can't coerce annotated object"))
    ((eqv? type (ar-type x)) x)
    ((char? x)      (case type
                      ((int)     (char->integer x))
                      ((string)  (string x))
                      ((sym)     (string->symbol (string x)))
                      (else      (err "Can't coerce" x type))))
    ((exint? x)     (case type
                      ((num)     x)
                      ((char)    (integer->char x))
                      ((string)  (apply number->string x args))
                      (else      (err "Can't coerce" x type))))
    ((number? x)    (case type
                      ((int)     (iround x))
                      ((char)    (integer->char (iround x)))
                      ((string)  (apply number->string x args))
                      (else      (err "Can't coerce" x type))))
    ((string? x)    (case type
                      ((sym)     (string->symbol x))
                      ((cons)    (arclist (string->list x)))
                      ((num)     (or (apply string->number x args)
                                     (err "Can't coerce" x type)))
                      ((int)     (let ((n (apply string->number x args)))
                                   (if n
                                       (iround n)
                                       (err "Can't coerce" x type))))
                      (else      (err "Can't coerce" x type))))
    ((mpair? x)     (case type
                      ((string)  (apply string-append
                                        (map (lambda (y) (ar-coerce y 'string))
                                             (racketlist x))))
                      (else      (err "Can't coerce" x type))))
    ((eqv? x 'nil)  (case type
                      ((string)  "")
                      (else      (err "Can't coerce" x type))))
    ((symbol? x)    (case type
                      ((string)  (symbol->string x))
                      (else      (err "Can't coerce" x type))))
    (else             x)))

(define (disp-to-string x)
  (let ((o (open-output-string)))
    (display x o)
    (close-output-port o)
    (get-output-string o)))

(define (ar-details c)
  (disp-to-string (exn-message c)))

(define (ar-len x)
  (cond ((string? x) (string-length x))
        ((hash? x) (hash-count x))
        (else (length (racketlist x)))))

(define (ar-maptable f table)
  (hash-for-each table f)
  table)

(define (ar-on-err errfn f)
  ((call-with-current-continuation
    (lambda (k)
      (lambda ()
        (with-handlers
         ((exn:fail? (lambda (c)
                       (k (lambda () (errfn c))))))
         (f)))))))

(test (ar-on-err
       (lambda (c) (ar-details c))
       (lambda () (err "foo")))
  "foo")

(define (ar-protect during after)
  (dynamic-wind (lambda () 't) during after))

(define (ar-scar x v)
  (set-mcar! x v)
  v)

(define (ar-scdr x v)
  (set-mcdr! x v)
  v)

(define (ar-disp x port)
  (display (fromarc x) port))

(define (ar-write x port)
  (write (fromarc x) port))

(define (ar-writec c . args)
  (write-char c
              (if (pair? args)
                  (car args)
                  (current-output-port)))
  c)

(define (ar-readb port)
  (let ((c (read-byte port)))
    (if (eof-object? c) 'nil c)))

(define (mcadr x)   (mcar (mcdr x)))
(define (mcddr x)   (mcdr (mcdr x)))
(define (mcdddr x)  (mcdr (mcddr x)))
(define (mcaddr x)  (mcar (mcddr x)))
(define (mcadddr x) (mcar (mcdddr x)))

(struct tagged (type rep) #:prefab)

(define arc-annotate tagged)

(define (ar-funcall f . args)
  (arc-apply f (arclist args)))

(define (apply-args x)
  (cond ((and (pair? x) (null? (cdr x)))
         (racketlist (car x)))
        ((pair? x)
         (cons (car x) (apply-args (cdr x))))
        ((null? x)
         '())
        (else
         x)))

(define (arc-apply f . rest)
  (let ((args (apply-args rest)))
    (cond ((procedure? f)
           (apply f args))
          ((hash? f)
           (call-table f args))
          ((string? f)
           (call-string f args))
          ((mpair? f)
           (call-list f (car args)))
          (else
           (error "not callable" (xhash f))))))

(define (call-table table args)
  (hash-ref table
            (car args)
            (if (null? (cdr args)) 'nil (cadr args))))

(define (call-string f args)
  (string-ref f (car args)))

(define (call-list lst k)
  (unless (>= k 0)
    (error "call-list: invalid index:" k))
  (if (= k 0)
       (mcar lst)
       (call-list (mcdr lst) (- k 1))))

(define (sref x k v)
  (cond ((hash? x)
         (hash-set! x k v))
        ((string? x)
         (string-set! x k v))
        ((mpair? x)
         (set-list x k v))
        (else
         (error "Can't set reference" x))))

(define (has g k)
  (unless (hash? g)
    (err "not a table" g))
  (tnil (hash-has-key? g k)))

(define (set-list x k v)
  (unless (>= k 0)
    (error "set-list: invalid index" k))
  (if (= k 0)
       (set-mcar! x v)
       (set-list (mcdr x) (- k 1) v)))

(define rand-chars
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define (rand-char)
  (string-ref rand-chars (random (string-length rand-chars))))

(define (rand-string n)
  (let ((s (make-string n)))
    (for ((i n))
      (string-set! s i (rand-char)))
    s))

(define (uniq . rest)
  (ar-coerce
    (string-append
      (if (pair? rest)
           (string-append (ar-coerce (car rest) 'string) "--")
           "")
      (rand-string 12))
    'sym))

(define (arc-close p)
  (cond ((input-port? p)  (close-input-port p))
        ((output-port? p) (close-output-port p))
        (else (error "Can't close" p))))

(define (ar->2 x y)
  (tnil (cond ((and (number? x) (number? y)) (> x y))
              ((and (string? x) (string? y)) (string>? x y))
              ((and (symbol? x) (symbol? y)) (string>? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char>? x y))
              (else (> x y)))))

(define (ar-<2 x y)
  (tnil (cond ((and (number? x) (number? y)) (< x y))
              ((and (string? x) (string? y)) (string<? x y))
              ((and (symbol? x) (symbol? y)) (string<? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char<? x y))
              (else (< x y)))))

(define (arc-map f xs)
  (if (eq? xs 'nil)
       'nil
       (mcons (f (mcar xs)) (arc-map f (mcdr xs)))))

(define (map-arclist->list f xs)
  (if (eq? xs 'nil)
       '()
       (cons (f (mcar xs))
             (map-arclist->list f (mcdr xs)))))

(define (deserialize x globals)
  (cond ((and (mpair? x) (eq? (mcar x) '$))
         (case (mcadr x)
           (($)        '$)
           ((globals)  globals)
           ((global)   (hash-ref globals (mcaddr x)
                         (lambda ()
                           (error "deserialize: global not found:" x))))
           ((tagged)   (arc-annotate
                         (mcaddr x)
                         (deserialize (mcadddr x) globals)))
           (else       (error "deserialize: invalid $:" x))))
        ((mpair? x)
         (mcons (deserialize (mcar x) globals)
                (deserialize (mcdr x) globals)))
        ((hash? x)
         (error "deserialize: oops, got a table:" x))
        (else
         x)))

(define (ar-racket-eval expr)
  (eval (toracket expr) namespace))

(define (xhash x)
  (cond ((eq? x rx)
         '$rx)
        ((mpair? x)
         (mcons (xhash (mcar x))
                (xhash (mcdr x))))
        ((pair? x)
         (cons (xhash (car x))
               (xhash (cdr x))))
        (else
         x)))

(define (runtime)
  (make-hasheqv
   `((annotate         . ,arc-annotate)
     (ar-arclist       . ,arclist)
     (ar-disp          . ,ar-disp)
     (ar-racket-eval   . ,ar-racket-eval)
     (ar-funcall       . ,ar-funcall)
     (ar-read          . ,arc-read)
     (ar-readb         . ,ar-readb)
     (ar-set           . ,set)
     (ar-string-append . ,string-append)
     (ar-toarc         . ,toarc)
     (ar-toarclist     . ,arclist)
     (ar-write         . ,ar-write)
     (ar->2            . ,ar->2)
     (ar-<2            . ,ar-<2)
     (apply            . ,arc-apply)
     (assert           . ,ar-assert)
     ; bogus atomic
     (atomic-invoke    . ,(lambda (f) (f)))
     (call-w/stdout    . ,(lambda (port thunk)
                            (parameterize ((current-output-port port))
                              (thunk))))
     (call-w/stdin     . ,(lambda (port thunk)
                            (parameterize ((current-input-port port))
                              (thunk))))
     (car              . ,arc-car)
     (cdr              . ,arc-cdr)
     (close            . ,arc-close)
     (coerce           . ,ar-coerce)
     (cons             . ,arc-cons)
     (details          . ,ar-details)
     (dir              . ,(lambda (name)
                            (toarc (map path->string (directory-list name)))))
     (dir-exists       . ,(lambda (name)
                            (if (directory-exists? name) name 'nil)))
     (disp             . ,display)
     (err              . ,err)
     (infile           . ,open-input-file)
     (has              . ,has)
     (is               . ,ar-is2)
     (len              . ,ar-len)
     (maptable         . ,ar-maptable)
     (mod              . ,modulo)
     (namefn           . ,(lambda (name f) (procedure-rename f name)))
     (newstring        . ,make-string)
     (on-err           . ,ar-on-err)
     (outfile          . ,open-output-file)
     (protect          . ,ar-protect)
     (quit             . ,exit)
     (rep              . ,tagged-rep)
     (scar             . ,ar-scar)
     (scdr             . ,ar-scdr)
     (sread            . ,arc-read)
     (sref             . ,sref)
     (stderr           . ,current-error-port)
     (stdin            . ,current-input-port)
     (stdout           . ,current-error-port)
     (t                . ,'t)
     (table            . ,table)
     (tagged           . ,(lambda (x) (tnil (tagged? x))))
     (type             . ,ar-type)
     (uniq             . ,uniq)
     (xhash            . ,xhash)
     (write            . ,write)
     (writec           . ,ar-writec)
     (+                . ,+)
     (-                . ,-)
     (*                . ,*)
     (/                . ,/))))

(define rx (runtime))

(define (load-precompiled filename)
  (let ((port (open-input-file filename))
        (eof (list 'eof)))
    (define (loop)
      (let ((e (arc-read port eof)))
        (unless (eq? e eof)
          (let ((d (deserialize e rx)))
            (ar-racket-eval d)
            (loop)))))
    (loop)
    (close-input-port port)))

(define (rxeval s)
  ((hash-ref rx 'eval) (toarc s)))

((lambda ()
  (load-precompiled "/tmp/arc.precompiled")
  (display "arc.precompiled loaded\n")

  (rxeval '(load 'arc/arc3
                 'arc/iso-table
                 'arc/iso-tagged
                 'common-tests
                 'runtime-tests
                 'more-tests))

  (void)))
