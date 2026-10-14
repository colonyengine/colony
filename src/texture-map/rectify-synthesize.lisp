(in-package #:colony.texture-map)

;; TODO: Temporary until I can decide on a better debugging/logging system.

(defmacro with-no-layout (&body body)
  `(let ((*print-right-margin* (* 1024 1024))
         (*print-miser-width* (* 1024 1024))
         (*print-length* (* 1024 1024))
         (*print-level* nil)
         (*print-circle* t)
         (*print-lines* nil))
     ,@body))

(defun debug-rectification (kind infer-style feature fmt &rest args)
  (with-no-layout
    (apply
     #'format t (concatenate 'string "rectification: [~S ~(~S~) ~(~S~)]: " fmt)
     kind infer-style feature args)))


;; ------------------------
;; Synthesize Methods
;; ------------------------

(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)

  ;; 0. Check that we can deduce any mipmaps at all!
  (u:when-let (delems (texmap:data-elements inst))
    (when (zerop (length delems))
      (error (u:cat
              "Rectify: Unexpectedly unable to deduce mipmaps during pass~%"
              " :deduce-mipmap-hierarchy for a texture-map-simple named: ~A")
             (texmap:name inst))))


  (debug-rectification "texture-map-simple"
                       infer-style
                       feature
                       "Attempting to deduce mipmaps for texture name: ~A~%"
                       (texmap:name inst))

  ;; 1. Deduce a new list of mipmap-* objects in order from largest to
  ;; smallest that can be observed from the data elements.
  (let* ((results (deduce-mipmap-hierarchy inst (texmap:style inst)
                                           (texmap:store inst)
                                           (texmap:store-args inst)
                                           :core core))
         (result (first results))
         (mipmaps (cdr result)))

    (debug-rectification "texture-map-simple"
                         infer-style
                         feature
                         "Deduced mipmap hierarchy:~%~{~{ ~S~%~}~%~}"
                         results)

    ;;(inspect results)

    (unless (= (length results) 1)
      (return-from rectify (values inst :unable-to-deduce-mipmaps)))

    ;; 0. Fixup the texture-maps' mipmaps array to hold the right number
    ;; of mipmaps that point to the right elidxs (if possible) for
    ;; synthesis. This will drop the original mipmaps array in the
    ;; texture-map which will get garbage collected.

    (when (> (length (texmap:mipmaps inst))
             (length mipmaps))
      (warn "TODO: Dropping some original mipmaps, is this bad?"))
    (setf (texmap:mipmaps inst)
          (adjust-array (texmap:mipmaps inst) (length mipmaps)))

    ;; 1. Copy over the attributes from the original mipmap to the new
    ;; one and place the new one in the inst's container replacing the
    ;; original one. We leave the orig mipmap instances to be GCed.
    (loop :for i :below (length (texmap:mipmaps inst))
          :for orig-mipmap = (aref (texmap:mipmaps inst) i)
          :for (extent new-mipmap) :in mipmaps
          ;; NOTE: This assumes the order the user wrote the mipmap
          ;; forms in the DSL are from largest to smallest.
          :do (abag:absorb new-mipmap :bags (list orig-mipmap))
              (setf (aref (texmap:mipmaps inst) i) new-mipmap))

    ;;(inspect inst)

    (values inst :ok)))

;; -------------------------------------------------------------------------
;; Cube maps are harder to do, so they get more code.

(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :cube))
                    (inst faces-representation)
                    (root texture-map-cube)
                    &key core)

  (format t (u:cat "rectify(~(~S~) ~(~S~) ~(~S~))): name: ~A "
                   "Checking faces are rectified...~%")
          infer-style feature (type-of inst) (texmap:name root))

  ;; The 6 texture-map-simple texture-maps used by this cube map varient
  ;; should already have been fully rectified. So, there is nothing to
  ;; really do here at this time other than check that this is true. The
  ;; :validation pass of rectification for this cube map instance will
  ;; determine if all the faces are compatible with each other, (square,
  ;; same size, same internal pixel format, etc), etc.

  (loop
    :with texmaptab = (colony::texture-maps core)
    :for face :across (texmap:faces inst)
    :for face-elidx = (texmap:elidx face)
    :for de = (aref (texmap:data-elements root) face-elidx)
    :for face-name = (texmap:logloc de)
    :for face-texmap-inst = (texmaptab::find-resolved-texture-map texmaptab
                                                                  face-name)
    :for face-texmap-inst-state = (texmap:state face-texmap-inst)
    :do (unless (eq (texmap:rectification-classification face-texmap-inst-state)
                    :rectified)
          (error (u:cat "Rectify: "
                        "While synthesizing cube map ~A, which is~%"
                        ":cube :faces, the cube map should have had face~%"
                        "~A already rectified. but the face isn't rectified!~%")
                 (texmap:name root) face-name)))

  (values inst :ok))


(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :cube))
                    (inst envmap-representation)
                    (root texture-map-cube)
                    &key core)
  (declare (ignore core))

  (format t (u:cat "rectify(~(~S~) ~(~S~) ~(~S~))): name: ~A Implement me!~%")
          infer-style feature (type-of inst) (texmap:name root))


  (values inst :ok))




;; Trampoline to a more specific method on the cube type.
(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-cube)
                    (root texture-map-cube)
                    &key core)

  (format t (u:cat "rectify(~(~S~) ~(~S~) ~(~S~))): name: ~A~%")
          infer-style feature (type-of inst) (texmap:name root))

  (rectify infer-style :cube (texmap:repr (texmap:cube inst)) root
           :core core)

  (values inst :ok))







(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-complex)
                    (root texture-map-complex)
                    &key core)
  (declare (ignore core))

  (format t (u:cat "rectify(~(~S~) ~(~S~) ~(~S~))): name: ~A~%")
          infer-style feature (type-of inst) (texmap:name root))

  ;; Just a hack for testing to return T.
  (values inst :ok))




;; There is a depth to this based on the store form which is not rendered here.
;;        style
;; model  | :unique | :combined | :faces | :envmap |
;; -------------------------------------------------
;;  :1d   |    X    |     X     |        |         |
;;  :2d   |    X    |     X     |        |         |
;;  :3d   |    X    |     X     |        |         |
;;  :cube |         |           |    X   |    X    |

;; Synthesis algorithm:
;;
;; ----
;;
;; 0. Deduce mipmap hierarchy from data-elements.
;; We process data-elements explicitly in order.
;; This deduces the mipmap hierarchy and actual location of the mipmap data
;; in the data-elements. It will ONLY fill in mipmaps for data that actually
;; exists via the materialized data-elements.
;;
;;  GF: (DEDUCE-MIPMAP-HIERARCHY texmap-inst model style store &key core)
;;
;;  NOTE: Understand this processes data-elements in order, which, in the
;;        context of synthesis is always largest to smallest mipmap.
;;
;;  This generic function returns a: <RESULT>
;;  <RESULT> ::=
;;               # when :1d, :2d, :3d, :cube/:envmap, name is self name.
;;               ((<NAME> <MIPMAP-OBS>+))
;;               # when :cube/:faces texture-maps, name is on behalf of a face.
;;             | ((<NAME> <MIPMAP-OBS>+)
;;                (<NAME> <MIPMAP-OBS>+)
;;                (<NAME> <MIPMAP-OBS>+)
;;                (<NAME> <MIPMAP-OBS>+)
;;                (<NAME> <MIPMAP-OBS>+)
;;                (<NAME> <MIPMAP-OBS>+))
;;  <MIPMAP-OBS> ::= (<EXTENT> <MIPMAP>)
;;                 | (<EXTENT> :dne)
;;  <MIPMAP> ::= mipmap-* clos object, filled in with real data.
;;  <NAME> ::= Name of texture map (a symbol)
;;  <EXTENT> ::= (<WIDTH> <HEIGHT> <DEPTH>)
;;  <WIDTH> ::= positive integer
;;  <HEIGHT> ::= positive integer
;;  <DEPTH> ::= positive integer
;;
;;  (deduce-mipmap-hierarchy texmap-1d :unique <store> <store-args>)
;;   Each data-element is one entire mipmap.
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-1d :combined <store> <store-args>)
;;   TODO
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-2d :unique <store> <store-args>)
;;   Each data-element is one entire mipmap.
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-2d :combined <store> <store-args>)
;;   TODO
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-3d :unique <store> <store-args>)
;;   Collect data-elements via store description until size changes,
;;    compute mipmap size for each size grouping.
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-3d :combined <store> <store-args>)
;;   TODO
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-cube :faces <store> <store-args>)
;;   For cube, process each sub-texture and collect results.
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-cube :envmap <store> <store-args>)
;;   TODO
;;   Return <RESULT>
;;
;; ----
;;
;; 1. Compute, given the base mipmap size, what all the resolutions for every
;; mipmap should be down to 1x1x1. (already implemented, just call it).
;;
;; ----
;;
;; 2. Zip 0 and 1 together, correctly setting sourced-p and sunk-p for
;; each mipmap and creating new empty mipmaps with sourced-p is nil for
;; those mipmap resolutions that didn't exist at all.
;;
;; ----
;;
;; 3. Adjust the size of the the texture-map's mipmaps array to match
;; the result of (2). Then change the texture-map's mipmap information
;; (including mapping spans) to match the corresponding entry in (2). If
;; there are not enough mipmaps in the texture-map, add more. If there
;; are too many, remove them. Same with mapping-spans in each mipmap.
;; Try and preserve the actual instances encountered by copying
;; information from (2) into the texture-map structure as opposed to
;; just changing the instance references. This tries to preserve any
;; reference aliases the appdev might have to things inside of the
;; texture-map.
;;
;; ----
;;
;; 5. Determine completion state by inspection of the texture-map's
;; reengineered mipmaps. Set it in the texture-map instance.
;;
;; ----
;;
;; 6. Mark texture-map's state as :synthesized.
;;
;; ----
;;
;; 7. Drop any references to used items so the GC can collect them. If this
;; becomes a problem, implement a pool for these types.

;; KEEP GOING

;; ----
;;
;; NOTE: When realizing an incomplete texture-map with mipmaps that are
;; not realizable (e.g. sourced-p on a mipmap is nil), use
;; glTexParameteri with GL_TEXTURE_BASE_LEVEL and GL_TEXTURE_MAX_LEVEL
;; set appropriately to fill in all holes that might exist in the mipmap
;; definition in order to complete the mipmap during realization. The
;; base level specified MUST exist for each gap being realized. So if 0,
;; 1, 2, 3 exist, then 8, 9, 10, exist, then GL_TEXTURE_BASE_LEVEL is
;; set to 3 and GL_TEXTURE_MAX_LEVEL is set to 7, and then
;; glGenerateMipmaps() to create the missing mipmaps. If there are other
;; holes, continue doing that method until everything is filled. This
;; operation leaves the mipmap(s) undefined and the texture-map
;; incomplete in terms of the CLOS objects. But realization ensures
;; there is a complete texture-map in the GPU.
