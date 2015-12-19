(mac assert (x)
  (list if x
         (list do (list ar-disp "OK " (list stdout))
                  (list ar-write (list quote x) (list stdout))
                  (list ar-disp #\newline (list stdout)))
         (list 'err "fail" (list quote x))))
