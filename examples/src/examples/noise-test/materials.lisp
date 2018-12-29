(in-package :first-light.example)

(fl:define-material noise-test
  (:profiles (fl.materials:u-mvpt)
   :shader fl.gpu.user:noise-test))
