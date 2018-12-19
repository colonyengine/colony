(in-package :first-light.example)

(define-scene sprite-test ()
  (@camera
   ((transform :translation/current (flm:vec3 0 0 1))
    (camera :activep t :mode :orthographic)))
  (@ship
   ((transform :rotation/current (flm:vec3 0 0 (/ pi -2))
               :scale/current (flm:vec3 0.5))
    (simple-movement)
    (shot-emitter))
   (@ship-body
    ((sprite-sheet :spec-resource-id :spritesheet-data
                   :material 'sprite
                   :animations (make-sprite-sheet-animations 0 0 #(#(1 "ship29")))))
    (@exhaust
     ((transform :translation/current (flm:vec3 0 -140 0))
      (sprite-sheet :spec-resource-id :spritesheet-data
                    :material 'sprite
                    :animations (make-sprite-sheet-animations
                                 0 0
                                 #(#(0.5
                                     "exhaust03-01"
                                     "exhaust03-02"
                                     "exhaust03-03"
                                     "exhaust03-04"
                                     "exhaust03-05"
                                     "exhaust03-06"
                                     "exhaust03-07"
                                     "exhaust03-08")))))))))
