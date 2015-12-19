(with (x (annotate 'plex '(a b))
       y (annotate 'plex '(a b)))
  (test (is x y) nil)
  (test (iso x y) t))
