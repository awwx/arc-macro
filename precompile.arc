; At this point we're running in Arc 3.1.  Run the common tests as a
; way to check that the tests are correct.

; Some early tests use assert when we don't have test yet.

(def assert (x)
  (if x
       (prn "assertion ok")
       (do (prn "assertion fail")
           (quit 1))))

(= ar-disp disp)

(= source-dirs '("arc" "runtime-tests" "common-tests"))

(def default-arc-extension (file)
  (let file (coerce file 'string)
    (if (find #\. file)
         file
         (+ file ".arc"))))

(def sourcepath (name)
  (let name (string name)
    (aif (dir-exists name)
          (map [+ name "/" _] (dir name))
         (file-exists (default-arc-extension name))
          (list it)
         (some [file-exists (+ _ "/" (default-arc-extension name))]
               source-dirs)
          (list it)
          (err "source file not found:" name))))

(def xload files
  (each f (flat (map sourcepath (flat files)))
    (prn f)
    (load f)))

(xload '(xhash
         extend-def
         iso-table
         iso-tagged
         test2
         common-tests))

; These are Arc demo runtime functions (implemented in Racket)
; injected by the demo Arc to Racket compiler.  If we also implemented
; the Arc 3.1 compiler optimizations these might include ar-funcall0,
; ar-funcall1, etc.  Since they don't need to be in the globals there
; should probably be a different mechanism to inject them, but putting
; them in the globals is an easy way to inject them for now.

(= ar-funcall   (fn () 'placeholder))
(= ar-toarclist (fn () 'placeholder))

; Load the macro expander and the demo Arc to Racket compiler.

(xload '(ssyntax
         ssyntax.t
         macro
         macro.t
         ac-racket
         serialize))

; The demo runtime allows a `nil` value being stored in a table to be
; distinguished from a key not present, and includes a `has` function
; which reports whether a table contains the given key.  The bogus
; implementation fails for a global variable being set to `nil`, but
; since we don't do that in the macro code this is enough to bootstrap
; on.

(def bogus-has (g k)
  (isnt (g k) nil))

; todo just rand-string could be the primitive

(def ar-uniq ((o name))
  (if name
       (coerce (+ (coerce name 'string) "--" (rand-string 12)) 'sym)
       (coerce (rand-string 12) 'sym)))

; An implementation in Arc 3.1 of the the globals which as also
; provided by the demo Arc runtime.

; Some of the runtime functions implement a subset of the
; functionality provided by Arc: for example, in `ar-disp` the port
; argument is required instead of being optional.  Since the
; functionality is a strict subset, we can get away with using the
; full Arc versions for the implementation.

(= globals
  (obj ar-disp          disp
       ar-funcall       ar-funcall
       ar-readb         readb
       ar-string-append +
       ar-toarclist     ar-toarclist
       ar-write         write
       ar-<2            <
       ar->2            >
       annotate         annotate
       apply            apply
       assert           assert
       atomic-invoke    atomic-invoke
       car              car
       cdr              cdr
       close            close
       coerce           coerce
       cons             cons
       details          details
       disp             disp
       err              err
       has              bogus-has
       infile           infile
       is               is
       len              len
       maptable         maptable
       mod              mod
       namefn           namefn
       newstring        newstring
       on-err           on-err
       outfile          outfile
       protect          protect
       quit             quit
       rep              rep
       scar             scar
       sread            sread
       sref             (fn (x k v) (sref x v k))
       ssexpand         ssexpand
       is-ssyntax       is-ssyntax
       stderr           stderr
       stdout           stdout
       t                t
       table            table
       tagged           tagged
       type             type
       uniq             ar-uniq
       write            write
       writec           writec
       xhash            xhash
       +                +
       -                -
       *                *
       /                /))

(= globals!bound
  (fn (var)
    (bogus-has globals var)))

(def precompile1 (infilename out)
  (w/infile in infilename
    (w/uniq eof
      (whiler a (read in eof) eof
        (let m (macro-expand a globals)
          (write (serialize (ac-racket m) globals) out)
          (disp "\n\n" out)
          (eval (unprimitive m)))))))

(def precompile (outfilename infilenames)
  (w/outfile out outfilename
    (each infilename infilenames
      (prn infilename)
      (precompile1 infilename out))))

(prn "precompiling...")

(let input-files (flat (map sourcepath (readfile "precompile.load-order")))
  (precompile "/tmp/arc.precompiled" input-files))

(prn "precompilation done")
