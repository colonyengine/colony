(in-package #:virality.examples)

;;; Textures

(v:define-texture damaged-helmet/metallic-roughness (:texture-2d)
  (:data #((:damaged-helmet-textures "metal-roughness.tiff"))))

(v:define-texture damaged-helmet/color (:texture-2d)
  (:data #((:damaged-helmet-textures "albedo.tiff"))))

(v:define-texture damaged-helmet/normal (:texture-2d)
  (:data #((:damaged-helmet-textures "normal.tiff"))))

(v:define-texture damaged-helmet/ambient-occlusion (:texture-2d)
  (:data #((:damaged-helmet-textures "ao.tiff"))))

(v:define-texture damaged-helmet/emissive (:texture-2d)
  (:data #((:damaged-helmet-textures "emissive.tiff"))))

;;; Materials

(v:define-material damaged-helmet
  (:shader ex/shd:damaged-helmet
   :profiles (x/mat:u-mvp)
   :uniforms
   ((:metallic-roughness-values (v2:vec 1))
    (:metallic-roughness-sampler 'damaged-helmet/metallic-roughness)
    (:base-color-sampler 'damaged-helmet/color)
    (:base-color-factor (v4:vec 1))
    (:normal-sampler 'damaged-helmet/normal)
    (:normal-scale 1f0)
    ;; NOTE: This vector points TOWARDS the light.
    (:light-direction (v3:vec 0f0 1f0 1f0))
    (:light-color (v3:vec 1))
    (:occlusion-sampler 'damaged-helmet/ambient-occlusion)
    (:occlusion-strength 1f0)
    (:emissive-sampler 'damaged-helmet/emissive)
    (:emissive-factor 0.3f0))))

;;; Prefabs

(v:define-prefab "damaged-helmet" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args) :zoom 10f0))
  (("helmet" :copy "/mesh")
   (c/xform:transform :rotate (q:orient :local :x (float (/ pi 2f0) 1.0))
                      :rotate/inc (o:make-velocity v3:+forward+
                                                   (float (- (* pi 1/6)) 1f0))
                      :scale 4f0)
   (c/smesh:static-mesh :asset '(:mesh "damaged-helmet.glb"))
   (c/render:render :material 'damaged-helmet)))

;;; Prefab descriptors

(v:define-prefab-descriptor damaged-helmet ()
  ("damaged-helmet" examples))
