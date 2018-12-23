(in-package :first-light.example)

(fl:define-material graph-test
  (:profiles (fl.materials:u-mvpt)
   :shader fl.shader:graph-test))
