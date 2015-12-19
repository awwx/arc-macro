; Begin bootstrapping.
;
; (mac global (var)
;   `(,globals ',var))
;
; -->
;
; (sref globals 'global
;   (annotate 'mac
;     (cons 'globals
;       (cons (cons 'quote (cons var nil))
;         nil))))
;
; -->

((globals ($quote--xVrP8JItk2Ot sref))
  globals
  ($quote--xVrP8JItk2Ot global)
  ((globals ($quote--xVrP8JItk2Ot annotate))
    ($quote--xVrP8JItk2Ot mac)
    ($fn--xVrP8JItk2Ot (var)
      ((globals ($quote--xVrP8JItk2Ot cons))
       globals
       ((globals ($quote--xVrP8JItk2Ot cons))
        ((globals ($quote--xVrP8JItk2Ot cons))
         ($quote--xVrP8JItk2Ot $quote--xVrP8JItk2Ot)
         ((globals ($quote--xVrP8JItk2Ot cons))
          var
          ($quote--xVrP8JItk2Ot nil)))
        ($quote--xVrP8JItk2Ot nil))))))
