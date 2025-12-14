(in-package #:colony.texture-map)

;;; ---------------------------------------------------------------------------
;;; BELOW is classification of the inference requirements of the in
;;; memory texture-map structure.

(defun collapse-inference-types (&rest items)
  "This function expects that ITEMS is a list of results from
CLASSIFY-RECTIFICATION. It returns :synthesize, :validate, or :infer,
depending on the contents of the ITEMS list."
  (cond
    ((every (u:curry #'eq :synthesize) items)
     :synthesize)
    ((every (u:curry #'eq :validate) items)
     :validate)
    (t
     :infer)))

(defgeneric classify-rectification (inst root &key core &allow-other-keys)
  (:documentation
   "Classify INST and return one of four choices:
 nil         - Unclassifiable by the CLASSIFY-RECTIFICATION system.
 :synthesize - ALL values/structure must be synthesized from base knowledge.
 :validate   - ALL values/structure are specified and must be validated.
 :infer      - SOME values/structure must be filled in. May need backtracking.

If there is a choice between :synthesize and :validate, then :infer is chosen.
If there is a choice between :infer and someting else, :infer is chosen.

CLASSIFY-RECTIFICATION is often called on data structures with many sub
pieces that themselves have subpieces and this generic function is
expected to recurse on those pieces. ROOT should be the root level
object of that tree. Often when exxamining INST (which is some possibly
deep subpart of ROOT), you'll need access to ROOT to compute the
classification. The keyword argument :CORE is usually the core instance
of the engine, since sometimes classification can cause references to
other data to be found in CORE as well. It is very likely you'll start
the CLASSIFY-RECTIFICATION with the same object for INST and ROOT. This
is normal."))

(defmethod classify-rectification (inst root &key core)
  (declare (ignore inst root core))
  nil)

(defmethod classify-rectification ((inst (eql nil)) root &key core)
  (declare (ignore inst root core))
  :synthesize)

(defmethod classify-rectification ((items vector) root &key core)
  (apply #'collapse-inference-types
         (map 'list (u:rcurry #'classify-rectification root :core core) items)))

;; ---------- texture-map processing

(defmethod classify-rectification ((inst data-span) (root texture-map)
                                   &key core)
  (declare (ignore root core))
  ;; We assume the elidx is previously correct.
  (let ((origin-infer-type (if (texmap:origin inst)
                               :validate
                               :synthesize))
        (extent-infer-type (if (texmap:extent inst)
                               :validate
                               :synthesize)))
    (collapse-inference-types origin-infer-type extent-infer-type)))

(defmethod classify-rectification ((inst span) (root texture-map) &key core)
  (declare (ignore root core))
  (let ((origin-infer-type (if (texmap:origin inst)
                               :validate
                               :synthesize))
        (extent-infer-type (if (texmap:extent inst)
                               :validate
                               :synthesize)))
    (collapse-inference-types origin-infer-type extent-infer-type)))

(defmethod classify-rectification ((inst mapping-span) (root texture-map)
                                   &key core)
  (let ((to-infer-type
          (classify-rectification (texmap:to inst) root :core core))
        (from-infer-type
          (classify-rectification (texmap:from inst) root :core core)))
    (collapse-inference-types to-infer-type from-infer-type)))

(defmethod classify-rectification ((inst mipmap) (root texture-map) &key core)
  (let ((extent-infer-type
          (classify-rectification (texmap:extent inst)
                                  root :core core))
        (mapping-spans-infer-type
          (classify-rectification (texmap:mapping-spans inst)
                                  root :core core)))
    (collapse-inference-types extent-infer-type mapping-spans-infer-type)))

;; ---------- texture-map-simple processing

(defmethod classify-rectification ((inst texture-map-simple)
                                   (root texture-map-simple)
                                   &key core)
  ;; We understand that the other fields in the inst are corrrectly and fully
  ;; specified.
  (classify-rectification (texmap:mipmaps inst) root :core core))

;; ---------- texture-map-complex processing

(defmethod classify-rectification ((inst face)
                                   (root texture-map-complex)
                                   &key core)
  ;; Find the name of the face, look it up in the textable...
  (let* ((texmap-table (colony::texture-maps core))
         (delems (texmap:data-elements root))
         (face-name (texmap:logloc (aref delems (texmap:elidx inst))))
         (face-texture-map-inst
           (texmaptab::find-resolved-texture-map texmap-table face-name))
         (texmap-state (texmap:state face-texture-map-inst))
         ;; ...and observe it's classification
         (rect-class (texmap:rectification-classification texmap-state)))

    ;; TODO: This is shady a little bit. Figure out exactly when this is
    ;; ok to do and when it is expected to be done. (Changing a cube map
    ;; when the simple-textures all have been previously rectified might
    ;; be sufficient, but the full understanding of these consequences
    ;; is not complete.)
    (if (eq rect-class :rectified)
        :validate
        rect-class)))

(defmethod classify-rectification ((inst faces-representation)
                                   (root texture-map-complex)
                                   &key core)
  (classify-rectification (texmap:faces inst) root :core core))

(defmethod classify-rectification ((inst envmap-representation)
                                   (root texture-map-complex)
                                   &key core)
  ;; NOTE: Cube mipmaps are specified in the manner as in a
  ;; texture-map-simple and don't use texture-map-elements, but instead
  ;; image-elements in the data-element array. So, we simply classify
  ;; them like regular mipmaps.
  (classify-rectification (texmap:mipmaps inst) root :core core))

(defmethod classify-rectification ((inst cube)
                                   (root texture-map-complex)
                                   &key core)
  (classify-rectification (texmap:repr inst) root :core core))

(defmethod classify-rectification ((inst texture-map-complex)
                                   (root texture-map-complex)
                                   &key core)
  ;; This assumes all simple texture-maps referenced by this object have been
  ;; classified BEFORE this texture-map has been classified.
  (classify-rectification (texmap:cube inst) root :core core))

;;; ABOVE is classify-rectification and associated functions
