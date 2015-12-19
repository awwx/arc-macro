(extend-def iso (x y) (and (tagged x) (tagged y)
                           (is (type x) (type y))
                           (iso (rep x) (rep y)))
  t)
