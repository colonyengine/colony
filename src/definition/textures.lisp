(in-package #:virality.contrib.textures)

(tex:define-texture framebuffer-color (:procedural framebuffer))

(tex:define-texture framebuffer-depth (:procedural framebuffer)
  (:internal-format :depth-component)
  (:pixel-format :depth-component))

(tex:define-texture framebuffer-stencil (:procedural framebuffer)
  (:internal-format :stencil-index)
  (:pixel-format :stencil-index))

(tex:define-texture framebuffer-depth/stencil (:procedural framebuffer)
  (:internal-format :depth24-stencil8)
  (:pixel-format :depth-stencil)
  (:data-type :unsigned-int-24-8))

(tex:define-texture debug-texture (:texture-2d clamp-all-edges)
  ;; I can put overrides in here too specific to this texture.
  (:data #(((:core :texture) "debug-0.tiff")
           ((:core :texture) "debug-1.tiff")
           ((:core :texture) "debug-2.tiff")
           ((:core :texture) "debug-3.tiff")
           ((:core :texture) "debug-4.tiff")
           ((:core :texture) "debug-5.tiff")
           ((:core :texture) "debug-6.tiff")
           ((:core :texture) "debug-7.tiff")
           ((:core :texture) "debug-8.tiff")
           ((:core :texture) "debug-9.tiff")
           ((:core :texture) "debug-10.tiff"))))
