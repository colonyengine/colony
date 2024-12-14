(in-package #:colony.extension)

(texmap:define-texture-map debug-texture (:2d :unique)
  (texmap:mipmap (c::textures c::debug-0))
  (texmap:mipmap (c::textures c::debug-1))
  (texmap:mipmap (c::textures c::debug-2))
  (texmap:mipmap (c::textures c::debug-3))
  (texmap:mipmap (c::textures c::debug-4))
  (texmap:mipmap (c::textures c::debug-5))
  (texmap:mipmap (c::textures c::debug-6))
  (texmap:mipmap (c::textures c::debug-7))
  (texmap:mipmap (c::textures c::debug-8))
  (texmap:mipmap (c::textures c::debug-9))
  (texmap:mipmap (c::textures c::debug-10)))

;; TODO: TMAP Convert :data to reference define-texture-map name
(tex:define-texture debug-texture (:texture-2d clamp-all-edges)
  ;; I can put overrides in here too specific to this texture.
  (:data #((c::textures c::debug-0)
           (c::textures c::debug-1)
           (c::textures c::debug-2)
           (c::textures c::debug-3)
           (c::textures c::debug-4)
           (c::textures c::debug-5)
           (c::textures c::debug-6)
           (c::textures c::debug-7)
           (c::textures c::debug-8)
           (c::textures c::debug-9)
           (c::textures c::debug-10))))

(defmacro gen-define-textures (pool-name prefix options &body name-list)
  `(progn
     ,@(loop
         :for name :in name-list
         :append
         `((texmap:define-texture-map
               ,(u:format-symbol :colony.extension
                                 "~A~A" prefix name)
               (:2d :unique)
             (texmap:mipmap (,pool-name ,(u:format-symbol :c "~A" name))))

           (tex:define-texture
               ,(u:format-symbol :colony.extension
                                 "~A~A" prefix name)
               ,options
             ;; TODO: TMAP Convert :data to reference above texture-map name.
             (:data #((,pool-name ,(u:format-symbol :c "~A" name)))))))))

;; These textures are CC-0 from the Blender Foundation.
(gen-define-textures c:matcaps matcap/ (:texture-2d clamp-all-edges)
  basic-1
  basic-2
  basic-dark
  basic-side
  ceramic-dark
  ceramic-lightbulb
  check-normal-y
  check-rim-dark
  check-rim-light
  clay-brown
  clay-muddy
  clay-studio
  jade
  metal-anisotropic
  metal-carpaint
  metal-lead
  metal-shiny
  pearl
  reflection-check-horizontal
  reflection-check-vertical
  resin
  skin
  toon)
