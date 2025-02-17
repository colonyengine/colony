(in-package #:colony.texture-map)

;; Given the texmap-set (which is a hashtable whose keys are either texture-map
;; DSL names or actual texture-map data structures, load all of the data into
;; main memory via the resource-cache.
;;
;; TODO: Currently, we just materialize everything possible as opposed to
;; partial materialization and pushing the remaining work off to a future
;; frame.



(defun materialize (core texmap-names &key force)
  "Materialize all texture-maps specified in the list TEXMAP-NAMES.
The elements of TEXMAP-NAMES must be symbols which name texture-maps which
have been either previously reified or registered during runtime with CORE. The
result of this operation is that each texture-map instance is verified that all
of its data-element assets are present, that the mipmaps/cube spans are valid,
that each element in the data-elements instance points to the correct
(and possibly shared) cache-item, and that all of the actual element data
is in main memory. For any texture-maps where this cannot be true, the symbols
representing the names of those texture-maps are returned in a list. If the
return value is NIL, it meant all texture-maps were materialized properly."

  ;; This uses the resource-cache, so these higher level tasks are broken
  ;; down into concurrent tasks and shoved through the scheduler.

  (let ((texmap-table (colony::texture-maps core))
        (materializing-texmaps (u:dict #'eq))
        (unmaterializable-texture-maps nil))

    ;; 0: Convert the texture-map names to a texture-map instance via the
    ;;    texture-map-table in CORE. We wlll only process ones that need
    ;;    materialization or we're forcing them.
    (dolist (name texmap-names)
      (let ((texmap (texmaptab::find-resolved-texture-map texmap-table name)))
        (if texmap
            (when (or (not (texmap:materialized-p (texmap:state texmap)))
                      force)
              (setf (u:href materializing-texmaps name) texmap))
            (pushnew name unmaterializable-texture-maps))))

    ;; 1: Check that all elements of the texture-maps are actually present.

    ;; 2. Reserve cache-items for those elements.

    ;; 3. Load entirely each element and satisfy the reservations along the
    ;; way.

    ;; 4. Generate and/or verify that the mipmaps/spans or cube representation
    ;; are all actually valid in this texture-map given the header info of each
    ;; element.

    ;; 5. Assign the elements slot in the data-elements to point to the
    ;; cache-item. There will be one cache-item per element, and multiple
    ;; texture-maps may reference it.

    ;; Mark the texture-map-state that everything is ready and materialized.

    unmaterializable-texture-maps))
