; (def literal (x)
;   (case (type x)
;     sym   (in x nil t)
;     cons  (or (caris x quote) (caris x 'quote))
;           t))

(def literal (x)
  (if (isa x 'sym)
       (or (is x nil) (is x t))
      (isa x 'cons)
       (or (caris x quote) (caris x 'quote))
       t))