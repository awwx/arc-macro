(def literal-value (x)
  (if (or (isa x 'sym) (isa x 'cons))
       (if (is x 'nil)
            nil
           (is x 't)
            t
           (or (caris x quote) (caris x 'quote))
            (cadr x)
           (err "not a literal value" x))
       x))
