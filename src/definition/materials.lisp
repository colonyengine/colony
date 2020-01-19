(in-package #:virality.extensions.materials)

(mat:define-material unlit-color
  (:shader shd/tex:unlit-color
   :profiles (u-mvp)))

(mat:define-material unlit-color-decal
  (:shader shd/tex:unlit-color-decal
   :profiles (u-mvp)))

(mat:define-material unlit-texture
  (:shader shd/tex:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'x/tex:debug-texture)
              (:mix-color (v4:one)))))

(mat:define-material unlit-texture-decal
  (:shader shd/tex:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (v4:zero))
              (:max-intensity (v4:one))
              (:tex.sampler1 'x/tex:debug-texture))))

(mat:define-material unlit-texture-decal-bright
  (:shader shd/tex:unlit-texture-decal
   :profiles (u-mvp)
   :uniforms ((:min-intensity (v4:vec 0.1f0 0.1f0 0.1f0 0.1f0))
              (:max-intensity (v4:one))
              (:tex.sampler1 'x/tex:debug-texture))))

(mat:define-material sprite
  (:profiles (u-mvp)
   :shader shd/sprite:sprite
   :uniforms ((:sprite.sampler 'x/tex:debug-texture)
              (:opacity 1.0))
   :blocks ((:block-name :spritesheet
             :storage-type :buffer
             :block-alias :spritesheet
             :binding-policy :manual))))

(mat:define-material missing-material
  (:shader shd/tex:unlit-texture
   :profiles (u-mvp)
   :uniforms ((:tex.sampler1 'x/tex:debug-texture))))

(mat:define-material collider/sphere
  (:shader shd/vis:collider/sphere
   :profiles (u-mvp)
   :uniforms ((:collider-local-center (v3:zero))
              (:in-contact-color (v4:vec 1f0 0f0 0f0 1f0))
              (:not-in-contact-color (v4:vec 0f0 1f0 0f0 .5f0))
              (:in-contact-p nil)
              (:radius 0f0))))

(mat:define-material collider/cuboid
  (:shader shd/vis:collider/cuboid
   :profiles (u-mvp)
   :uniforms ((:collider-local-center (v3:zero))
              (:in-contact-color (v4:vec 1f0 0f0 0f0 1f0))
              (:not-in-contact-color (v4:vec 0f0 1f0 1f0 .5f0))
              (:in-contact-p nil)
              (:minx 0f0)
              (:maxx 0f0)
              (:miny 0f0)
              (:maxy 0f0)
              (:minz 0f0)
              (:maxz 0f0))))
