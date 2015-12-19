(def set-equal (s1 s2)
  (and (is (len s1) (len s2))
       (all [some _ s2] s1)))

(def table-equal (t1 t2)
  (and (set-equal (keys t1) (keys t2))
       (all [iso t1._ t2._] (keys t1))))

(extend-def iso (x y) (and (isa x 'table) (isa y 'table))
  (table-equal x y))
