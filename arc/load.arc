(def loadfile (file into)
  (prn file)
  (w/infile f file
    (w/uniq eof
      (whiler e (read f eof) eof
        (eval e into)))))

(def default-arc-extension (file)
  (let file (coerce file 'string)
    (if (find #\. file)
         file
         (+ file ".arc"))))

(def load0 (name into)
  (if (dir-exists name)
       (map [loadfile (+ name "/" _) into] (dir name))
       (loadfile (default-arc-extension name) into)))

(def load names
  (let into globals
    (each name names
      (if (in (type name) 'string 'sym)
           (load0 (string name) into)
           (= into name)))))
