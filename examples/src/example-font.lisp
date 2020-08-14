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

;; The basic reusable text-display, :copy or :link this into other prefabs and
;; make changes as needed.
(v:define-prefab "text-display" (:library examples)
  (comp:geometry :name 'comp::text)
  (comp:font :asset '(metadata font)
             :text "Hello, World!")
  (comp:render :material 'font
               :slave (v:ref :self :component 'comp:geometry)))

;; Example use of the above prefab.
(v:define-prefab "example-text-display" (:library examples)
  (("camera" :copy "/cameras/ortho"))

  (("sign" :copy "/text-display")
   (comp:transform :scale (v3:vec 5f0)
                   :translate/velocity (v3:vec 0f0 50f0 0f0))
   (comp:font :text "Wall Clock Time"))

  (("wall-clock-time" :copy "/text-display")
   (comp:transform :scale (v3:vec 5f0))
   (comp:font :rate 0
              :text (lambda ()
                      (format nil "~5$"
                              (v:total-time (v:context (v:ref :self))))))))
