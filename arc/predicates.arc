(def no (x)
  (is x nil))

(def acons (x)
  (is (type x) 'cons))

(def atom (x)
  (no (acons x)))

(def isnt (x y) (no (is x y)))
