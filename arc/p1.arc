(mac p (x)
  (let gx (uniq)
    (list let gx x
      (list ar-write (list quote x) (list stdout))
      (list ar-disp  ": "           (list stdout))
      (list ar-write gx             (list stdout))
      (list ar-disp  #\newline      (list stdout))
      gx)))
