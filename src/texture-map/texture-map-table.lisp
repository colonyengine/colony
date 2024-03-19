(in-package #:colony.texture-map.texture-map-table)

;;;; Implementation of datatype TEXTURE-MAP-TABLE

(defun make-texture-map-table (&rest init-args)
  (apply #'make-instance 'texture-map-table init-args))

(defun add-semantic-texture-map-descriptor (texmapdesc texture-map-table)
  (setf (u:href (semantic-texture-map-descriptors texture-map-table)
                (texmap:name texmapdesc))
        texmapdesc))

(defun remove-semantic-texture-map-descriptor (texmapdesc texture-map-table)
  (remhash texmapdesc (semantic-texture-map-descriptors texture-map-table)))

(defun find-semantic-texture-map-descriptor (semantic-texture-map-name
                                             texture-map-table)
  (u:href (semantic-texture-map-descriptors texture-map-table)
          semantic-texture-map-name))
