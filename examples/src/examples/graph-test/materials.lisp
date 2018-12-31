(in-package :first-light.example)

(fl:define-material graph-test
  (:profiles (fl.materials:u-mvpt)
   :shader fl.gpu.user:graph-test))

(fl:define-material 3d-graph-test
  (:profiles (fl.materials:u-mvpt)
   :shader fl.gpu.user:3d-graph-test/1
   :instances 1000
   :attributes (:depth :always)
   :uniforms
   ((:size 1)
    (:min 0)
    (:by 1))))
