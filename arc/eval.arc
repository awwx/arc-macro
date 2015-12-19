(def eval (expr (o into globals))
  (let m (macro-expand expr into)
    (let r (ac-racket m)
      (ar-racket-eval r))))
