(in-package :first-light.example)

(define-options ()
  :title "First-Light Examples"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug
  :log-repl-categories nil #++'(:fl.core.texture))
