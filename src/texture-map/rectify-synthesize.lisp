(in-package #:colony.texture-map)

;; ------------------------
;; Synthesize Methods
;; ------------------------

(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :number-of-mipmaps/unique))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)

  (declare (ignore infer-style feature inst root core))
  ;; Deduce the mipmap structure from the
  ;; data-elements/model/style/store. Assume data-elements are in
  ;; descending mipmap size order during synthesis. Validate this fact.
  ;; The deduction is robust to all texture maps types, though we are
  ;; restricted here to texture-map-simple types.

  ;; problem: in a 1d/2d situation, the data-elements will be EACH starting
  ;; at a larger size then going downwrds to the smallest mipmap.
  ;; But, in a 3d situation, you might have a set of IxJ mipmaps, then another
  ;; set of KxL mipmaps which are smaller, and so on. So, do we deduce that
  ;; here or do we split the

  ;; NOTE:
  ;; We assume that elidxs are a priori correct (but we may have to add
  ;; new mipmaps with new mapping-spans and we make sure those are correct).
  ;;
  ;; NOTE: zero data-elements is already checked for before this is called.
  ;;
  ;; Figure out how many mipmaps there should be in the texture-map.
  ;; If 1d,2d,
  ;;   If (= (length current-mipmaps) (length data-elements)), all good.
  ;;   If (< (length current-mipmaps) (length data-elements)), add mipmaps.
  ;;   If (> (length current-mipmaps) (length data-elements)), del mipmaps.
  ;;   (Any mipmaps we add must have the elidx slots set.)
  ;; else 3d
  ;;    If zero current-mipmaps,
  ;;       either we deduced them from the data-elements and make the right
  ;;       mipmap instances, or we error :unable-to-synthesize-mipmaps
  ;;    If there is one mipmap form, it must use all the data-elements.
  ;;    (Any mipmaps we add must have the elidx slots set.)
  ;;

  ;; Just a hack for testing to return T.
  (values t :ok))


(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :number-of-mipmaps/combined))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)

  (declare (ignore infer-style feature inst root core))
  ;; Deduce the mipmap structure from the
  ;; data-elements/model/style/store. Assume data-elements are in
  ;; descending mipmap size order during synthesis. The deduction is
  ;; robust to all texture maps types, though we are restricted here to
  ;; texture-map-simple types.


  ;; NOTE:
  ;; We assume that elidxs are a priori correct.
  ;;
  ;; 0. If zero data-elements, error :missing-data-elements
  ;; 1. Find out (length current-mipmaps) currently specified.
  ;; 2. Find out (length data-elements)
  ;;
  ;; 3. Figure out how many mipmaps there should be in the texture-map.
  ;; if :combined,
  ;;    If 1d,2d:
  ;;      There must be one data-element.
  ;;      Guess, using :store, the number and location of mipmaps in the image.
  ;;      If there are zero mipmaps, maybe an error?
  ;;      If there is one mipmap, increase to guessed mipmap number.
  ;;      If num mipmaps = num guessed mipmaps, all good.
  ;;      If >0 mipmaps and <guessed-num, check :mipmap-combined-policy
  ;;      If >guessed-num, check :mipmap-combined-policy
  ;;    If 3d:
  ;;      KEEP GOING.

  ;; Just a hack for testing to return T.
  (values t :ok))

;; This only synthesizes the right number of mipmaps, potentially their
;; elidxs, and then returns.
(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :number-of-mipmaps))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)
  (declare (ignore feature))

  ;; 0. Check that we can synthesize any mipmaps at all!
  (u:when-let (delems (texmap:data-elements inst))
    (when (zerop (length delems))
      (return-from rectify (values nil :missing-data-elements))))

  ;; 1. Synthesize the mipmaps container contents and the elidx of each mipmap.
  (multiple-value-bind (result error-domain)
      ;; NOTE: This may change the mipmap array reference and size!
      (ecase (texmap:style inst)
        (:unique
         (rectify infer-style :number-of-mipmaps/unique
                  inst root :core core))
        (:combined
         (rectify infer-style :number-of-mipmaps/combined
                  inst root :core core)))
    (values result error-domain)))

(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :mipmap))
                    (inst mipmap)
                    (root texture-map-simple)
                    &key core)
  (declare (ignore infer-style feature inst root core))

  ;; Just a hack for testing to return T.
  (values t :ok))

(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)
  (declare (ignore feature))

  ;; We choose this algorithm to NOT be recursive in the way a lisper
  ;; might have written it. We instead process the INST in a breadth
  ;; first manner wrt the container of the mipmaps. We do this to reduce
  ;; the choices and depth of what would have been the recursive
  ;; algorithm. In this particular instance, it makes this more
  ;; understandable and maintainable.

  ;; 0. Fixup the mipmaps array to hold the right number of mipmaps that point
  ;; to the right elidxs (if possible) for synthesis.
  (multiple-value-bind (result error-domain)
      ;; The mipmaps vector could be reassigned by this method.
      (rectify infer-style :number-of-mipmaps inst root :core core)
    (unless result
      (return-from rectify (values result error-domain))))
  ;; 1. Walk each mipmap (now that we know how many we have and for sure
  ;; we know the elidxs are correct) and synthesize the information that
  ;; specific mipmap needs from the image data.
  (loop :for mipmap :across (texmap:mipmaps inst)
        :do (multiple-value-bind (result error-domain)
                (rectify infer-style :mipmap mipmap root :core core)
              (unless result
                (return-from rectify (values result error-domain)))))
  (values t :ok))




(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-complex)
                    (root texture-map-complex)
                    &key core)
  (declare (ignore feature root core))
  (format
   t "rectify(texture-map-complex, synthesize): ~(~S~) ~(~S~): Implement me!~%"
   (texmap:name inst) infer-style)
  ;; Just a hack for testing to return T.
  t)

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
;;  TODO: Make deduce-mipmap-hierarchy a generic function.
;;  NOTE: Understand this processes data-elements in order, which, in the
;;        context of synthesis is always largest to smallest mipmap.
;;
;;  This generic function returns a: <RESULT>
;;  <RESULT> ::=
;;               # when :1d, :2d, :3d, :cube/:envmap, name is self name.
;;               ((<NAME> <TEXMAP-OBS>))
;;               # when :cube/:faces texture-maps, name is each face.
;;             | ((<NAME> <TEXMAP-OBS>)
;;                (<NAME> <TEXMAP-OBS>)
;;                (<NAME> <TEXMAP-OBS>)
;;                (<NAME> <TEXMAP-OBS>)
;;                (<NAME> <TEXMAP-OBS>)
;;                (<NAME> <TEXMAP-OBS>))
;;  <TEXMAP-OBS> ::= (<MIPMAP>+)
;;                 | NIL
;;  <MIPMAP> ::= mipmap-* clos object, filled in with real data.
;;  <NAME> ::= Name of texture map (a symbol)
;;
;;  (deduce-mipmap-hierarchy texmap-1d :unique <store>)
;;   Each data-element is one entire mipmap.
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-1d :combined <store>)
;;   TODO
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-2d :unique <store>)
;;   Each data-element is one entire mipmap.
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-2d :combined <store>)
;;   TODO
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-3d :unique <store>)
;;   Collect data-elements via store description until size changes,
;;    compute mipmap size for each size grouping.
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-3d :combined <store>)
;;   TODO
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-cube :faces <store>)
;;   For cube, process each sub-texture and collect results.
;;   Return <RESULT>
;;
;;  (deduce-mipmap-hierarchy texmap-cube :envmap <store>)
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
;; 5. Determine complete-p by inspection of the texture-map's
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
