(in-package #:virality.contrib.materials)

(mat:define-material unlit-color
  (:shader shd.tex:unlit-color
   :profiles (u-mvp)))

(mat:define-material unlit-color-decal
  (:shader shd.tex:unlit-color-decal
   :profiles (u-mvp)))

(mat:define-material unlit-texture
  (:shader shd.tex:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'contrib.tex:debug-texture)
              (:mix-color (v4:one)))))

(mat:define-material unlit-texture-decal
  (:shader shd.tex:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (v4:zero))
              (:max-intensity (v4:one))
              (:tex.sampler1 'contrib.tex:debug-texture))))

(mat:define-material unlit-texture-decal-bright
  (:shader shd.tex:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (v4:vec 0.1 0.1 0.1 0.1))
              (:max-intensity (v4:one))
              (:tex.sampler1 'contrib.tex:debug-texture))))

(mat:define-material sprite
  (:profiles (u-mvp)
   :shader shd.sprite:sprite
   :uniforms ((:sprite.sampler 'contrib.tex:debug-texture)
              (:opacity 1.0)
              (:alpha-cutoff 0.1))
   :blocks ((:block-name :spritesheet
             :storage-type :buffer
             :block-alias :spritesheet
             :binding-policy :manual))))

(mat:define-material missing-material
  (:shader shd.tex:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'contrib.tex:debug-texture))))

(mat:define-material collider/sphere
  (:shader shd.vis:collider/sphere
   :profiles (u-mvp)
   :uniforms ((:collider-local-position (v3:zero))
              (:in-contact-color (v4:vec 1 0 0 1))
              (:not-in-contact-color (v4:vec 0 1 0 0.5))
              (:in-contact-p nil)
              (:radius 0.0))))
