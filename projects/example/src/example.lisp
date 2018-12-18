(in-package :first-light.example)

(define-options ()
  :title "First-Light Examples"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug)

(in-package :%first-light)

(define-graph :fl.example
    (:category :component-dependency
     :depends-on ((:core (all-unknown-types core-types)))
     :roots (all-ordered-types))
  (subdag all-ordered-types
          ((splice core-types)
           -> (splice all-unknown-types))))

(define-graph :fl
    (:category :component-package-order
     :depends-on ((:core-component-order (core-packages)))
     :roots (start-search))
  (subdag current-project (:fl.example.comp.* -> :fl.example))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
