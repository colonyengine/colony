(in-package #:virality-examples)

;;; Textures

(v:define-texture font (:texture-2d)
  (:data #((textures font))))

;;; Materials

(v:define-material font
  (:shader ex/shd:font
   :profiles (x:u-mvp)
   :uniforms
   ((:sampler 'font)
    (:color (v4:vec 0 1 0 0.75)))))

;;; Prefabs

(v:define-prefab "font" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("text")
   (comp:transform :scale (v3:vec 2))
   (comp:geometry :name 'comp::text)
   (comp:font :asset '(metadata font)
              :text "Hello, World!")
   (comp:render :material 'font
                :slave (v:ref :self :component 'comp:geometry))))
