(in-package #:xXx-SYSTEM-NAME-xXx)

;; c:define-texture have no ordering dependencies between themselves.
;; c:define-texture must come before c:define-material
;; c:define-materials have no ordering dependencies between themselves.
;; TODO: c:define-model order at this load location.
;; c:define-prefab DOES have an ordering between themselves.


;; This acts like a scene, but is just a regular prefab.
(c:define-prefab "initial-scene" (:library lib/main)
  (("camera" :copy "/cameras/perspective")
   (comp:transform :translate (v3:vec 0f0 0f0 5f0))
   (comp:camera :zoom 3f0))
  (("cube-0" :copy "/v-cube")
   (flip-material :material-array (vector 'v-letter 'v-letter-invert)
                  :flip-hz 2f0
                  :renderer (c:ref "/initial-scene/cube-0/cube"
                                   :component 'comp:render))
   (comp:transform :translate (v3:vec -2f0 0f0 0f0)))
  (("cube-1" :copy "/v-cube")
   (comp:transform :translate (v3:vec 2f0 0f0 0f0))))
