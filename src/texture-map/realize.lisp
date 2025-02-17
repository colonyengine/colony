(in-package #:colony.texture-map)

;; "Realize" means transfer previously materialized texture-map data to the
;; GPU

;; TODO: This often requires additional information, like what texture id to
;; use and whatnot. So the interface will change when I get here in the control
;; flow.

;; Put the texture-map in memory image data onto the GPU.

(defun realize (core texmap-set)
  "TEXMAP-SET may be a single symbol naming a texture-map, or an actual
texture-map in memory object, hash table"
  (declare (ignore core texmap-set))

  nil
  )
