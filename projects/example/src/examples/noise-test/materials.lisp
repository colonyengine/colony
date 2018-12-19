(in-package :first-light.example)

(define-material noise-test
  (:profiles (fl.materials:u-mvpt)
   :shader fl.shader:noise-test))
