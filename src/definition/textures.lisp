(in-package #:virality.extensions.textures)

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
  (:data #((:virality/texture "debug-0.png")
           (:virality/texture "debug-1.png")
           (:virality/texture "debug-2.png")
           (:virality/texture "debug-3.png")
           (:virality/texture "debug-4.png")
           (:virality/texture "debug-5.png")
           (:virality/texture "debug-6.png")
           (:virality/texture "debug-7.png")
           (:virality/texture "debug-8.png")
           (:virality/texture "debug-9.png")
           (:virality/texture "debug-10.png"))))
