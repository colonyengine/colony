(in-package #:virality.contrib.materials)

(v:define-material unlit-color
  (:shader first-light.shader.texture:unlit-color
   :profiles (u-mvp)))

(v:define-material unlit-color-decal
  (:shader first-light.shader.texture:unlit-color-decal
   :profiles (u-mvp)))

(v:define-material unlit-texture
  (:shader first-light.shader.texture:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'contrib.tex:debug-texture)
              (:mix-color (v4:one)))))

(v:define-material unlit-texture-decal
  (:shader first-light.shader.texture:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (v4:zero))
              (:max-intensity (v4:one))
              (:tex.sampler1 'contrib.tex:debug-texture))))

(v:define-material unlit-texture-decal-bright
  (:shader first-light.shader.texture:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (v4:vec 0.1 0.1 0.1 0.1))
              (:max-intensity (v4:one))
              (:tex.sampler1 'contrib.tex:debug-texture))))

(v:define-material sprite
  (:profiles (u-mvp)
   :shader first-light.shader.sprite:sprite
   :uniforms ((:sprite.sampler 'contrib.tex:debug-texture)
              (:opacity 1.0)
              (:alpha-cutoff 0.1))
   :blocks ((:block-name :spritesheet
             :storage-type :buffer
             :block-alias :spritesheet
             :binding-policy :manual))))

(v:define-material missing-material
  (:shader first-light.shader.texture:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'contrib.tex:debug-texture))))

(v:define-material collider/sphere
  (:shader first-light.shader.visualization:collider/sphere
   :profiles (u-mvp)
   :uniforms ((:collider-local-position (v3:zero))
              (:in-contact-color (v4:vec 1 0 0 1))
              (:not-in-contact-color (v4:vec 0 1 0 0.5))
              (:in-contact-p nil)
              (:radius 0.0))))
