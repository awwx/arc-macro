; (assign mac
;   (annotate 'mac
;     (fn (name parms . body)
;       `(,assign ,name (,annotate 'mac (,fn ,parms ,@body))))))
;
; todo sig, safeset

(assign mac
  (annotate 'mac
    (fn (name parms . body)
      (list do
            (list assign name
              (list annotate ''mac
                (cons fn (cons parms body))))))))
