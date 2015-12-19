(test (pos #\x "abcdef")   nil)
(test (pos #\c "abcdef")   2)
(test (pos #\c "abcdef" 2) 2)
(test (pos #\c "abcdef" 3) nil)
