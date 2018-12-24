(in-package :first-light.example)

(fl:define-material sprite
  (:profiles (fl.materials:u-mvp)
   :shader fl.shader:sprite-test
   :uniforms
   ((:tex.image 'sprites)
    (:tex.sprite 0))
   :blocks
   ((:block-name :sprite-sheet
     :storage-type :buffer
     :block-alias 'ssbo/specification-data
     :binding-policy :manual))))
