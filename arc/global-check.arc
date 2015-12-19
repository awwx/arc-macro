(mac global (var)
  `(,do (,if (,no (,bound ',var))
              (,err "global var not defined:" ',var))
        (,globals ',var)))
