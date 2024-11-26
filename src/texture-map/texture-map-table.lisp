(in-package #:colony.texture-map.texture-map-table)

;;;; Implementation of datatype TEXTURE-MAP-TABLE

(defun make-texture-map-table (&rest init-args)
  (apply #'make-instance 'texture-map-table init-args))

;;; Semantic texture-map API

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

;;; Resolved texture-map API

(defun add-resolved-texture-map (texture-map-table texture-map)
  (setf (u:href (resolved-texture-maps texture-map-table)
                (texmap:name texture-map))
        texture-map))

(defun remove-resolved-texture-map (texture-map-table texture-map-or-name)
  (declare (ignore texture-map-table texture-map-or-name))
  ;; Careful, may have to replace references to map with debug maps or replace
  ;; only the materialized data with a debug map copy, think about it.  Also,
  ;; remove by name or by actual instance.
  (error "TODO: Implement me."))

(defun find-resolved-texture-map (texture-map-table texture-map-name)
  (u:href (resolved-texture-maps texture-map-table)
          texture-map-name))

(defun resolve-semantic-texture-map-descriptor (texture-map-table
                                                texture-map-desc)
  ;; Actually construct the in memory representation of the texture-map
  ;; object and jam it into the table. This will later be processed by
  ;; the engine and completed (menaing, all inferred information realized
  ;; and data possibly loaded from disk/network and put onto gpu, etc.
  ;;
  ;; Since we construct it whole sale here, there is no need for cloning
  ;; the in memory texture-map data structure.
  (let* ((main-texture-map (funcall (texmap:constructor texture-map-desc))))
    (add-resolved-texture-map texture-map-table main-texture-map)))

(defun resolve-all-semantic-texture-map-descriptors (texture-map-table)
  "Iterate through the semantic texture maps descriptors in the
TEXTURE-MAP-TABLE, and resolve each texture-map they describe, including
internal texture maps (as for cube maps) into an instance that can be used in
the application."
  (u:do-hash-values (desc (semantic-texture-map-descriptors texture-map-table))
    (resolve-semantic-texture-map-descriptor texture-map-table desc)))

(defun reify-texture-map-descriptors (core)
  "Clone all the meta descriptions of the texture-maps-descriptors into the
core. Then resolve them by constructing the texture-map in memory
data-structures. and put them into the RESOLVED-TEXTURE-MAP slot."
  (with-accessors ((texture-map-table c::texture-maps)) core
    (let ((eql-map (clone::make-eql-map)))
      (u:do-hash-values (desc c::=meta/texture-maps=)
        (let ((cloned-desc (clone:clone-deep desc eql-map)))
          (texmaptab:add-semantic-texture-map-descriptor cloned-desc
                                                         texture-map-table)))
      (texmaptab:resolve-all-semantic-texture-map-descriptors
       texture-map-table))))
