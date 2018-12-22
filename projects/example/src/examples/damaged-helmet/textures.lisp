(in-package :first-light.example)

(define-texture damaged-helmet/metallic-roughness (:texture-2d)
  (:data #((:damaged-helmet-textures "metal-roughness.tiff"))))

(define-texture damaged-helmet/color (:texture-2d)
  (:data #((:damaged-helmet-textures "albedo.tiff"))))

(define-texture damaged-helmet/normal (:texture-2d)
  (:data #((:damaged-helmet-textures "normal.tiff"))))

(define-texture damaged-helmet/ambient-occlusion (:texture-2d)
  (:data #((:damaged-helmet-textures "ao.tiff"))))

(define-texture damaged-helmet/emissive (:texture-2d)
  (:data #((:damaged-helmet-textures "emissive.tiff"))))
