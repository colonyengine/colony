(in-package #:colony-examples)

;;; Texture Maps
(c:define-texture-map font (:2d :unique)
  (texmap:mipmap (textures font)))

;;; Textures
(c:define-texture font (:texture-2d)
  ;; TODO: TMAP Fix to accept texture-map name.
  (:data #((textures font))))

;;; Materials

(c:define-material font
  (:shader ex/shd:font
   :profiles (x:u-mvp)
   :uniforms
   ((:sampler 'font)
    (:color (v4:vec 0f0 1f0 0f0 0.75f0)))))

;;; Prefabs

;; The basic reusable text-display, :copy or :link this into other prefabs and
;; make changes as needed.
(c:define-prefab "default-text-display" (:library examples)
  ("text-container"
   (comp:geometry :name 'comp::text)
   (comp:font :asset '(metadata font)
              :text "Hello, World!")
   (comp:render :material 'font
                :slave (c:ref :self :component 'comp:geometry))))

;; Example use of the above prefab.
(c:define-prefab "text-wall-clock-time" (:library examples)
  (("camera" :copy "/cameras/ortho"))

  (("sign" :copy "/default-text-display")
   (comp:transform :scale (v3:uniform 5f0)
                   :translate (v3:vec 0f0 256f0 0f0))
   ;; override child component info
   ("text-container"
    (comp:font :text "Wall Clock Time")))

  (("wall-clock-time" :copy "/default-text-display")
   (comp:transform :scale (v3:uniform 5f0))
   ;; override child component info
   ("text-container"
    (comp:font :rate 0
               :text (lambda ()
                       (format nil "~5$"
                               (c:total-time (c:context (c:ref :self)))))))))

;; TODO: Make a "text-driver" component and put into toplevel of
;; "text-display" It has references to everything else and will delegate
;; setting :text :rate and whatever else. This is so all the machinery in
;; the "text-display" is hidden behind a single API for the user to use.
