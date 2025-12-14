(in-package #:colony.texture-map)

;;; ---------------------------------------------------------------------------
;;; MATERIALIZE, the entry point into the entire above
;;; pipeline. This will load all registered texture maps into memory and
;;; ensure that the texture-map in-memory instances are all up to date
;;; and all their slots are filled.

(defun materialize (core texmap-names &key force)
  "Materialize all texture-maps specified in the list TEXMAP-NAMES.
The elements of TEXMAP-NAMES must be symbols which name texture-maps which have
been either previously reified or registered during runtime with CORE. The
result of this operation is that each texture-map instance is verified that all
of its data-element assets are present, that the mipmaps/cube spans are valid,
that each element in the data-elements instance points to the correct
(and possibly shared) cache-item, and that all of the actual element data is in
main memory. The state of the texture-map is set to :materialized. For any
texture-maps where this cannot be true, the symbols representing the names of
those texture-maps are returned in a list. If the return value is NIL, it meant
all texture-maps were materialized properly."

  (multiple-value-bind (materialized-texmap-insts unregistered-texmap-names)
      ;; Phase 1: Materialize ALL the texture-map's data-elements into
      ;; main-memory
      (materialize-data-elements core texmap-names :force force)

    (format t "materialize: materialized texture-map instances (names): ~A~%"
            (mapcar #'texmap:name materialized-texmap-insts))

    (format t "materialize: unregistered texture-map names: ~A~%"
            unregistered-texmap-names)

    ;; We split the simple maps from the complex maps and process the
    ;; simple ones first during the next phases. Since complex
    ;; texture-maps depend on simple texture-maps (at this time), it
    ;; gives us a natural way to order the processing.
    (multiple-value-bind (tm-simple-list tm-complex-list)
        (u:partition (lambda (tmap)
                       (subtypep (type-of tmap) 'texture-map-simple))
                     materialized-texmap-insts)

      ;; Phase 2: Classify ALL the materialized texture-maps into one of
      ;; four rectification inference categories: :synthesize,
      ;; :validate, :infer, or nil.
      (dolist (texmap-insts (list tm-simple-list tm-complex-list))
        (dolist (texmap-inst texmap-insts)
          (let ((tmap-state (texmap:state texmap-inst))
                (infer-style
                  (classify-rectification texmap-inst texmap-inst :core core)))
            (format t "CLASS-RECTI: ~(~S~) <- ~(~S~) ~(~S~)~%"
                    infer-style
                    (texmap:name texmap-inst)
                    (list (texmap:model texmap-inst)
                          (texmap:style texmap-inst)
                          (texmap:store texmap-inst)))
            (setf (texmap:rectification-classification tmap-state)
                  infer-style))))

      ;; Phase 3: Rectify ALL the texture-maps. This will concretize all
      ;; required values in the texture-map instance. Rectification will
      ;; hop onto this path in the diagram below at the appropriate
      ;; state and do the work until rectification is complete or there
      ;; is an error.
      ;;
      ;; :synthesize -> :validate--+--> recified-p is T
      ;; ^                      ^  |
      ;; |                      |  +--> ERROR
      ;; +------>:infer<--------+
      (dolist (texmap-insts (list tm-simple-list tm-complex-list))
        (dolist (texmap-inst texmap-insts)
          (let* ((texmap-state (texmap:state texmap-inst))
                 (infer-style
                   (texmap:rectification-classification texmap-state)))
            (rectify infer-style :texture-map
                     texmap-inst texmap-inst :core core))))))

  :todo-return-something-good-here
  )
