(test (map [if (is _ #\b) #\X _] "abc") "aXc")

(test (map + '(1 2 3) '(10 20 30)) '(11 22 33))
