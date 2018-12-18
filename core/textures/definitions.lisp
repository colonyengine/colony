(in-package :first-light.textures)

(define-texture framebuffer-color (:procedural framebuffer))

(define-texture framebuffer-depth (:procedural framebuffer)
  (:internal-format :depth-component)
  (:pixel-format :depth-component))

(define-texture framebuffer-stencil (:procedural framebuffer)
  (:internal-format :stencil-index)
  (:pixel-format :stencil-index))

(define-texture framebuffer-depth/stencil (:procedural framebuffer)
  (:internal-format :depth24-stencil8)
  (:pixel-format :depth-stencil)
  (:data-type :unsigned-int-24-8))

(define-texture debug-texture (:texture-2d clamp-all-edges)
  ;; I can put overrides in here too specific to this texture.
  (:data #(((:core :texture) "debug-0.tga")
           ((:core :texture) "debug-1.tga")
           ((:core :texture) "debug-2.tga")
           ((:core :texture) "debug-3.tga")
           ((:core :texture) "debug-4.tga")
           ((:core :texture) "debug-5.tga")
           ((:core :texture) "debug-6.tga")
           ((:core :texture) "debug-7.tga")
           ((:core :texture) "debug-8.tga")
           ((:core :texture) "debug-9.tga")
           ((:core :texture) "debug-10.tga"))))

(define-texture missing-texture (:texture-2d clamp-all-edges)
  (:data #(((:core :texture) "missing.tga"))))
