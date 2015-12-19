; Now that we have iso, a simple version of test that doesn't print
; the expressions.

; (mac test (expr expected)
;   `(,assert (,iso ,expr ,expected)))

(mac test (expr expected)
  (list assert (list iso expr expected)))
