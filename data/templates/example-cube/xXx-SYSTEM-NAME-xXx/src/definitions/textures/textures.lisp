(in-package #:xXx-SYSTEM-NAME-xXx)

(c:define-texture-map v-letter (:single :unique)
  (:mipmap () (textures v-letter)))

(c:define-texture v-letter (:texture-2d x:clamp-all-edges)
  ;; TODO: TMAP Fix this to use the texture-map name.
  (:data #((textures v-letter))))
