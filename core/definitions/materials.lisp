(in-package :first-light.materials)

(fl:define-material unlit-color
  (:shader fl.gpu.texture:unlit-color
   :profiles (u-mvp)))

(fl:define-material unlit-color-decal
  (:shader fl.gpu.texture:unlit-color-decal
   :profiles (u-mvp)))

(fl:define-material unlit-texture
  (:shader fl.gpu.texture:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'fl.textures:debug-texture)
              (:mix-color (m:vec4 1)))))

(fl:define-material unlit-texture-decal
  (:shader fl.gpu.texture:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (m:vec4))
              (:max-intensity (m:vec4 1))
              (:tex.sampler1 'fl.textures:debug-texture))))

(fl:define-material unlit-texture-decal-bright
  (:shader fl.gpu.texture:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (m:vec4 0.1))
              (:max-intensity (m:vec4 1))
              (:tex.sampler1 'fl.textures:debug-texture))))

(fl:define-material sprite
  (:profiles (u-mvp)
   :shader fl.gpu.sprite:sprite
   :uniforms ((:sprite.sampler 'fl.textures:debug-texture)
              (:opacity 1.0)
              (:alpha-cutoff 0.1))
   :blocks ((:block-name :spritesheet
             :storage-type :buffer
             :block-alias :spritesheet
             :binding-policy :manual))))

(fl:define-material missing-material
  (:shader fl.gpu.texture:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'fl.textures:debug-texture))))

(fl:define-material collider/sphere
  (:shader fl.gpu.visualization:collider/sphere
   :profiles (u-mvp)
   :uniforms ((:collider-local-position (m:vec3))
              (:in-contact-color (m:vec4 1 0 0 1))
              (:not-in-contact-color (m:vec4 0 1 0 .5))
              (:in-contact-p nil)
              (:radius 0.0))))
