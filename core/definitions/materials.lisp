(in-package :first-light.materials)

(fl:define-material unlit-color
  (:shader fl.shader:unlit-color
   :profiles (u-mvp)))

(fl:define-material unlit-color-decal
  (:shader fl.shader:unlit-color-decal
   :profiles (u-mvp)))

(fl:define-material unlit-texture
  (:shader fl.shader:unlit-texture
   :profiles (u-mvp)
   :uniforms
   ((:tex.sampler1 'fl.textures:debug-texture)
    (:mix-color (flm:vec4 1)))))

(fl:define-material unlit-texture-decal
  (:shader fl.shader:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms
   ((:min-intensity (flm:vec4))
    (:max-intensity (flm:vec4 1))
    (:tex.sampler1 'fl.textures:debug-texture))))

(fl:define-material unlit-texture-decal-bright
  (:shader fl.shader:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms
   ((:min-intensity (flm:vec4 0.1))
    (:max-intensity (flm:vec4 1))
    (:tex.sampler1 'fl.textures:debug-texture))))

;; NOTE: If there is a problem looking up a material, this is what a material
;; will become to indicate there is a problem.
(fl:define-material missing-material
  (:shader fl.shader:unlit-texture
   :profiles (u-mvp)
   :uniforms
   ((:tex.sampler1 'fl.textures:missing-texture))))
