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
  ;; NOTE: We clone the individual texture-maps when resolving them because
  ;; later, during live coding, we'll have a much better chance of being able
  ;; to figure out how to integrate a new (or changed!) texture-map-descriptor
  ;; in the face of mutations done to the resolved texture-maps during the
  ;; execution of the app itself.

  (let* ((eql-map (clone:make-eql-map))
         (main-texture-map
           (clone:clone-deep (texmap:ast texture-map-desc) eql-map)))

    ;; There is always a texture-map in the AST.
    (add-resolved-texture-map texture-map-table main-texture-map)

    ;; But cube maps MAY have extra-texture-maps depending if they are
    ;; anonymously defined. If there are any, they are promoted to first-class
    ;; resolved texture maps.
    (when (texmap::extra-asts-p texture-map-desc)
      (dolist (extra-texture-map (texmap::extra-asts texture-map-desc))
        (add-resolved-texture-map texture-map-table
                                  (clone:clone-deep extra-texture-map
                                                    eql-map))))))

(defun resolve-all-semantic-texture-map-descriptors (texture-map-table)
  "Iterate through the semantic texture maps descriptors in the
TEXTURE-MAP-TABLE, and resolve each texture-map they describe, including
internal texture maps (as for cube maps) into an instance that can be used in
the application."
  (u:do-hash-values (desc (semantic-texture-map-descriptors texture-map-table))
    (resolve-semantic-texture-map-descriptor texture-map-table desc)))
