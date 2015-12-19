; Now that we have quasiquotation, a more informative assert.

(mac assert (x)
  `(if ,x
        (do (ar-disp "OK " (stdout))
            (ar-write ',x (stdout))
            (ar-disp #\newline (stdout)))
       (err "FAIL" ',x)))
