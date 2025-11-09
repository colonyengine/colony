(in-package #:colony.texture-map)

;; In MATERIALIZE, given the texmap-set (which is a hashtable whose keys are
;; either texture-map DSL names or actual texture-map data structures, load all
;; of the data into main memory via the resource-cache.
;;
;; TODO: Currently, we just materialize everything possible as opposed to
;; partial materialization and pushing the remaining work off to a future
;; frame.

;; TODO: It is probably true that this should move to a more generic location
;; in the engine, since there could be other reason why we'd want to invoke
;; this state machine.

(defun dump-info (info)
  (format t "Dumping warming-info:~%")
  (rc:map-events info
                 (lambda (ct-inst event-list)
                   ;; opaque data has a weird format.
                   (format t " Texture-map: ~A~%  data-element: ~A~%  ct ~A~%"
                           (texmap:name (car (rc:opaque-data ct-inst)))
                           (logloc (cdr (rc:opaque-data ct-inst)))
                           ct-inst)
                   (format t "  events:~%")
                   (dolist (event (reverse event-list))
                     (format t "   ~S~%" event)))))


;; image element state machine

(defmethod rc:reserve-caching-task ((caching-task caching-task/image-element)
                                    resource-cache-scheduler resource-cache)
  (let ((cache-item (rc:make-cache-item
                     :opaque-data caching-task
                     :policy :unlocked
                     :state :reserved
                     :location :cl-heap)))
    (setf (apply #'rc:rcref
                 resource-cache
                 (rc:domain-id caching-task)
                 ;; This key is a list
                 (rc:key caching-task))
          cache-item)

    (rc:record-event
     (rc:info caching-task) caching-task `(:reserved ,cache-item))

    (values :reserved caching-task)))

(defmethod rc:recycle-caching-task ((caching-task caching-task/image-element)
                                    cache-item
                                    resource-cache-scheduler)
  (setf (rc:opaque-data cache-item) caching-task
        (rc:policy cache-item) :unlocked
        (rc:state cache-item) :reserved
        (rc:location cache-item) :cl-heap
        (rc:size cache-item) nil
        (rc:core cache-item) nil)

  (when (rc:value cache-item)
    ;; TODO: Figure out if I need to free this resource or just drop it
    ;; like I am doing here and let the GC get it if noone else has
    ;; reference.
    (setf (rc:value cache-item) nil))

  (rc:record-event
   (rc:info caching-task) caching-task `(:recycled ,cache-item))

  (values :reserved caching-task))


(defmethod rc:compute-caching-task ((caching-task caching-task/image-element)
                                    resource-cache-scheduler)
  (let* ((context (colony::context (rc::core caching-task)))
         ;; 1. convert asset key to a physical asset-path
         (asset (car (rc:key caching-task)))
         (asset-path (c:with-asset-cache context :texture asset
                       (c::resolve-path asset)))
         ;; 2. load filepath as generic image from asset-path
         (img (img:load-image asset-path)))

    ;; 3. Finally store the img results (and where we actually found
    ;; it) into caching task....
    (setf (rc:value caching-task) img
          (texmap:physloc caching-task) asset-path)

    (rc:record-event
     (rc:info caching-task) caching-task `(:computed ,img))

    (values :computed caching-task)))

(defmethod rc:synchronize-from-caching-task ((caching-task
                                              caching-task/image-element)
                                             resource-cache-scheduler)
  (let ((resource-cache (c::resource-cache (rc:core caching-task)))
        (data-element
          ;; TODO This ad hoc opaque-data structure is scary. Maybe make it
          ;; a real object.
          (cdr (rc:opaque-data caching-task))))
    (lock:with-lock (resource-cache)
      (let ((cache-item (rc:lookup-caching-task caching-task
                                                resource-cache-scheduler
                                                resource-cache)))
        (unless cache-item
          (dump-info (rc:info caching-task))

          (error "ERROR: There should be a cache item for caching-task ~A with key-list ~A but there is not!"
                 caching-task
                 (rc:key caching-task)))

        ;; Synchronize the information in the caching-task into the
        ;; cache-item (and also into the data-element).
        (let ((img (rc:value caching-task)))
          (setf
           ;; fill in cache-item
           (rc:value cache-item) img
           (rc:state cache-item) :cached
           ;; TODO: add size computation to image class. this is
           ;; wrong since it doesn't take into consideraton the
           ;; size of a pixel.
           (rc:size cache-item) (* (img:width img)
                                   (img:height img))
           ;; We use the opaque-data in the cache-item to store the
           ;; physloc.
           (rc:opaque-data cache-item) (texmap:physloc caching-task)

           ;; Now fill in the data-element fields for which this
           ;; caching-task was doing its work. This connects the
           ;; data-element directly to the cache-item.
           (texmap:physloc data-element) (texmap:physloc caching-task)
           (texmap:element data-element) cache-item)

          (rc:record-event
           (rc:info caching-task) caching-task :synchronize-from)

          (values :synchronized caching-task))))))

(defmethod rc:synchronize-to-caching-task ((caching-task
                                            caching-task/image-element)
                                           cache-item
                                           resource-cache-scheduler)
  ;; NOTE: We stored the physloc of the logloc into the opaque-data in
  ;; the cache-item. Lets use it to fix up the data-element.
  (let ((data-element (cdr (rc:opaque-data caching-task))))
    (setf (texmap:physloc data-element) (rc:opaque-data cache-item)
          (texmap:element data-element) cache-item)

    (rc:record-event
     (rc:info caching-task) caching-task :synchronize-to)

    (values :synchronized caching-task)))

(defmethod rc:discard-caching-task ((caching-task caching-task/image-element)
                                    resource-cache-scheduler)
  (rc:record-event
   (rc:info caching-task) caching-task :discarded)

  (error "rc:discard-caching-task: not implemented yet")

  (values :synchronized caching-task))

(defmethod rc:dispose-caching-task ((caching-task caching-task/image-element)
                                    resource-cache-scheduler)

  ;; The image was handed off to the cache-item, so just drop the reference.
  (setf (rc:value caching-task) nil)

  (rc:record-event
   (rc:info caching-task) caching-task :disposed)

  (values :disposed caching-task))

;;
;; TODO: Add matcl subset and u:doseq
;;

(defgeneric submit-data-elements (texmap-inst scheduler info)
  (:documentation "Pick all of the data-elements from the TEXMAP-INST and
submit them into the scheduler. For texture-map-cube instances, it will
look up each texture-map for each face and submit those data-elements as
appropriate as well. Return two values. The first value is a list of
TEXTURE-INST values which have been submitted. The second is a list of
TEXTURE-NAMES which could not be submitted because they weren't
registered."))

(defmethod submit-data-elements ((texmap-inst texture-map-simple)
                                 scheduler
                                 info)
  (loop :for elem :across (texmap:data-elements texmap-inst)
        :do (rc:acquire-caching-task
             scheduler
             'caching-task/image-element :texture-map
             :info info
             ;; TODO: This key feels like an impedance mismatch as a
             ;; key into the rc associated with the cache-item. What
             ;; exactly should go here? We're saying all logical
             ;; specifications map to the same cache-item--and this
             ;; seems right. But it still needs a little thinking.
             ;; Should there a protocol method specifically designed
             ;; in the warming protocol to generate a key from a
             ;; caching-task?
             :key (list (texmap:logloc elem))
             :core (rc:core scheduler)
             ;; TODO: This data seems like another impedance
             ;; mismatch. The warming protocol specific to this
             ;; caching-task often needs back-channels to the thing
             ;; wanting to warm the cache item, is there a better
             ;; way to represent this?
             :opaque-data (cons texmap-inst elem)))
  (values (list texmap-inst) nil))


(defmethod submit-data-elements ((texmap-inst texture-map-complex)
                                 scheduler
                                 info)
  (let ((texmap-table (colony::texture-maps (rc:core scheduler)))
        (submitted (list texmap-inst))
        (unsubmitted nil))
    (loop :for elem :across (texmap:data-elements texmap-inst)
          ;; BROKEN: Check define-texture-map does the right hting
          ;; with :faces and :envmap--cause right now it doesn't!
          :do (ecase (texmap:style (texmap:cube texmap-inst))
                (:envmap
                 ;; We have regular image-elements to load that hold the
                 ;; entire cube map. The asset should resolve to an image.
                 (rc:acquire-caching-task
                  scheduler 'caching-task/image-element :texture-map
                  :info info
                  :key (list (texmap:logloc elem))
                  :core (rc:core scheduler)
                  ;; TODO: This ad hoc structure can be annoying.
                  :opaque-data (cons texmap-inst elem)))

                (:faces
                 ;; The data-element has a name of another texture in it
                 ;; which is one of the faces of the cube. We submit
                 ;; each face's data elements.
                 (let ((face-texmap-name (texmap:logloc elem)))
                   (u:if-let ((face-texture-map-inst
                               (texmaptab::find-resolved-texture-map
                                texmap-table face-texmap-name)))
                     (progn
                       (submit-data-elements
                        face-texture-map-inst scheduler info)
                       (push face-texture-map-inst submitted))
                     (push face-texmap-name unsubmitted))))))
    (values submitted
            unsubmitted)))

(defun materialize-data-elements (core texmap-names &key force)
  "Materialize only the data-elements from wherever they should come as
dictated by the asset form in the data-elements and into main memory.
Return three values: The warming-info structure, a list of materialized
texture-map instances, and a list of unregistered texture-map names."
  (declare (ignore force))

  ;; This uses the resource-cache, so these higher level tasks are broken
  ;; down into concurrent tasks and shoved through the scheduler and the
  ;; warming-protocol.

  (format t "materialize: about to materialize texmap-names: ~A~%"
          texmap-names)

  (let* ((info (make-warming-info/texture-map))
         (scheduler (colony::resource-cache-scheduler core))
         (executor (colony::resource-cache-executor core))
         (texmap-table (colony::texture-maps core))
         (materializable-texmap-insts nil)
         (unregistered-texmap-names nil))

    ;; First, locate registered textures and submit their elements for
    ;; materialization
    (dolist (name texmap-names)
      (u:if-let ((texmap-inst
                  (texmaptab::find-resolved-texture-map texmap-table name)))
        (multiple-value-bind (submitted unsubmitted)
            (submit-data-elements texmap-inst scheduler info)
          (dolist (inst submitted)
            (push inst materializable-texmap-insts))
          (dolist (name unsubmitted)
            (push name unregistered-texmap-names)))
        (push name unregistered-texmap-names)))

    ;; Execute the entire materialization.
    ;; NOTE: Currently, we're using the sequential executor, so all of it must
    ;; complete before this returns.
    (rc:execute executor scheduler)

    ;; TODO: Inspect INFO and see if anything broke. For now, just print
    ;; out the INFO for all the texture-maps so we can see what
    ;; happened.

    (dump-info info)

    (dolist (texmap-inst materializable-texmap-insts)
      (setf (texmap:materialized-p (texmap:state texmap-inst)) t))

    (values materializable-texmap-insts
            unregistered-texmap-names)))

;;; ABOVE is materialize-data-elements and associated functions
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
;;; ---------------------------------------------------------------------------
;;; BELOW is rectification of the in memory texture-map
;;; structure.

;;
;; rectification:
;;
;; "rectify" means validate any information given to us, and if no
;; information is given to us, synthesize what information should be
;; there. If a "rectify" fails it means there is an inconsistancy.
;;
;; This unfortunately is a prolog-like unification problem that tries to
;; see if what the appdev specified in the texture-map DSL unifies with
;; the actual data-elements holding the data. However, to save time, I'm
;; just implementing a common subset of the functionality and bailing
;; when the unification get too complex.
;;
;; ALgorithm:
;; foreach texture-map
;;  rectify-data-elements: all delem must have the same pixel format
;;  rectify-total-mipmaps: tot num mipmaps wrt base img size is validated/synth
;;  foreach mipmap # mipmap number, expected mipmap size from base delem
;;     rectify-mipmap-extent: validate/synthesize mipmap extent
;;     rectify-total-mapping-spans: total number of mapping-spans val/synth
;;     foreach mapping-span # using elidx delem image
;;       rectify-mapping-span: val/synth mapping span
;;         rectify-mapping-span-to: val/synth destination span
;;         rectify-mapping-span-from: val/synth source span
;;       build mapping-span-to set coverage map
;;       ensure coverage map perfectly covers mipmap extent, no over/underlap
;;
;; For cube maps, do the above, but also sure all base level mipmaps are
;; the same size between the faces, and exactly square for :faces
;; representation.

;; TODO: Candidate for a generalized method appropriate for use in many places
;; and by the appdev. In what package would it be?
(defgeneric rectify (infer-style feature inst root
                     &key core &allow-other-keys)
  (:documentation "Rectify the INST so any required values inside of it are
concretized. INFER-STYLE can be :synthesize, :validate, or :infer and
this dictates what the RECTIFY method is going to do. FEATURE is
(usually) a symbol that indicates what particular feature of the INST
will be rectified. ROOT is usually the root object in the hierarchy of a
nested set of objects used when rectification information needs to span
more information that is just available in the INST. CORE is the usual
engine core if needed. Return two values: The first value is T or NIL
depending on if the required rectification was successful. The second
value is ignored f the first value is T, or it is a keyword indicating
a reason for the failure of the rectification."))

;; scavange
(defmethod rectify ((infer-style (eql :synthesize))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)
  ;; TODO: How do I collect errors and warnings in this method? Should
  ;; they be signaled conditions with restarts so the user can fix stuff
  ;; up or just collected and reported for later?
  (declare (ignore infer-style feature inst root core))

  t

  #++
  (rectify infer-style :pixel-format
           (texmap:data-elements inst) root :core core)

  #++
  (let ((mipmaps (texmap:mipmaps texmap-inst)))
    (multiple-value-bind (validp model extents reason)
        (expected-extents (deduce-mipmap-structure texmap-inst))

      ;; TODO: This next function either decides however many mipmaps are
      ;; present is correct, or it will fix the number to be correct.
      (rectify-total-mipmaps infer-style texmap-inst expected-extents)

      (loop :for mipmap :across mipmaps
            :for required-extent :in expected-extents
            :for mipmap-idx :by 1
            :do ;; This loop forces/check the mipmap extents to be in the
                ;; right order (if present) in the in-memory
                ;; representation. Otherwise it fills them in.
                (rectify-mipmap-extent infer-style texmap-inst mipmap
                                       required-extent)
                (rectify-total-mapping-spans infer-style texmap-inst mipmap)
                (loop :for mapping-span :across (texmap:mapping-spans mipmap)
                      :do (rectify-mapping-span infer-style texmap-inst
                                                mipmap mapping-span)
                          (let ((coverage-map (build-coverage-map mipmap)))
                            (unless (perfect-covering-p coverage-map)
                              (error
                               "Bad coverage-map: Texture-map: ~A, mipmap: ~A"
                               (texmap:name texmap-inst)
                               mipmap-idx)))))

      ;; TODO: don't modify this return value in the caller.
      (texmap:extent (aref mipmaps 0)))))




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
  ;; descending mipmap size order during synthesis. The deduction is
  ;; robust to all texture maps types, though we are restricted here to
  ;; texture-map-simple types.


  ;; NOTE: do different behavior depending on :unique or :combined.
  ;; We assume that elidxs are a priori correct (but we may have to add
  ;; new mipmaps with new mapping-spans and we make sure those are correct).
  ;;
  ;; 5. Figure out how many mipmaps there should be in the texture-map.
  ;; If 1d,2d,
  ;;   If (= (length current-mipmaps) (length data-elements)), all good.
  ;;   If (< (length current-mipmaps) (length data-elements)), add mipmaps.
  ;;   If (> (length current-mipmaps) (length data-elements)), del mipmaps.
  ;;   (Any mipmaps we add must have the elidx slots set.)
  ;; else 3d
  ;;    If zero current-mipmaps,
  ;;       either we deduced them and make the right mipmap instances,
  ;;       or we error :unable-to-synthesize-mipmaps
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


  ;; NOTE: do different behavior depending on :unique or :combined.
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
        :do (multiple-value-bind (result error_domain)
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

;; ------------------------
;; Validate Methods
;; ------------------------

(defmethod rectify ((infer-style (eql :validate))
                    (feature (eql :mspans-cover-extent))
                    (inst mipmap)
                    (root texture-map-simple)
                    &key core)
  (declare (ignore infer-style feature inst root core))
  ;; TODO: Implement this check if the mapping-spans perfectly cover a
  ;; mipmap extent.
  ;;
  ;; 1) sum 3d vol of mapping spans, must equal to mipmap extent
  ;; 2) All mapping spans must not extend outside mipmap extent
  ;; 3) all combinations of mapping spans must not intersect
  ;; 4) ...then the mapping spans exactly cover the mipmap extent

  ;; Just a hack for testing to return T.
  t)

(defmethod rectify ((infer-style (eql :validate))
                    (feature (eql :pixel-format))
                    (inst vector)
                    (root texture-map-simple)
                    &key core)
  "Return two values: if all the image-data-elements in the INST use the
same pixel-format, then return T and :OK. Otherwise return NIL and a
kayword indicating reason for the failure."
  (declare (ignore infer-style feature core))
  (loop :with pixel-format = nil
        :for delem :across inst
        :for delem-pixel-format = (img:pixel-format
                                   (rc:value (texmap:element delem)))
        :do (if pixel-format
                (unless (eql delem-pixel-format pixel-format)
                  (return-from rectify (values nil :pixel-format-mismatch)))
                (setf pixel-format delem-pixel-format)))
  (values t :ok))

(defmethod rectify ((infer-style (eql :validate))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)
  (declare (ignore feature))

  (u:mvlet ((result error-domain
                    (rectify infer-style :pixel-format
                             (texmap:data-elements inst) root :core core)))
    (format
     t "rectify(texture-map-simple, validate): ~(~S~) ~(~S~): finish me!~%"
     (texmap:name inst) infer-style)

    (values result error-domain)))

(defmethod rectify ((infer-style (eql :validate))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-complex)
                    (root texture-map-complex)
                    &key core)
  (declare (ignore feature root core))
  (error
   "rectify(texture-map-complex, validate): ~(~S~) ~(~S~): Implement me!~%"
   (texmap:name inst) infer-style)
  (values t :ok))

;; ------------------------
;; Infer Methods
;; ------------------------

(defmethod rectify ((infer-style (eql :infer))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-simple)
                    (root texture-map-simple)
                    &key core)
  (declare (ignore infer-style feature inst root core))
  (error "rectify(texture-map-simple, infer): ~(~S~) ~(~S~): Implement me!~%"
         (texmap:name inst) infer-style)
  nil)

(defmethod rectify ((infer-style (eql :infer))
                    (feature (eql :texture-map-contents))
                    (inst texture-map-complex)
                    (root texture-map-complex)
                    &key core)
  (declare (ignore feature root core))
  (error "rectify(texture-map-complex, infer): ~(~S~) ~(~S~): Implement me!~%"
         (texmap:name inst) infer-style)
  nil)

;; ------------------------
;; Rectification Entry Point
;; ------------------------

;; Main toplevel entry method for texture-map rectification.
(defmethod rectify (infer-style
                    (feature (eql :texture-map))
                    (inst texture-map)
                    (root texture-map)
                    &key core)
  ;; todo: :infer requires prolog-like backtracking, we leave
  ;; unimplemented for now.
  (when (eq infer-style :infer)
    (error "Not implemented! Unable to rectify an :infer instance: ~A"
           inst))
  (let ((texmap-state (texmap:state inst)))
    ;; TODO: Maybe add a :force keyword argument to force rectification?
    ;; Otherwise we could bail early because it had already been done.
    ;; We may need this for when we runtime mutate the texture
    ;; map--which may cause rectification to occur again.
    (setf (texmap:rectified-p texmap-state) nil)
    ;; If we must :synthesize the data, we accomplish that first.
    (when (eq infer-style :synthesize)
      (unless (rectify :synthesize :texture-map-contents inst root :core core)
        (error "Rectification synthesis failed: ~A" inst))
      ;; Update the classification to the next one we need to do.
      (setf (texmap:rectification-classification texmap-state) :validate
            infer-style :validate))
    ;; Then, either after synthesis, or because we only need to validate,
    ;; process :validate.
    (when (eq infer-style :validate)
      (unless (rectify :validate :texture-map-contents inst root :core core)
        (error "Rectification validation failed: ~A" inst))
      (setf (texmap:rectification-classification texmap-state) :rectified
            (texmap:rectified-p texmap-state) t
            infer-style :rectified))
    ;; If something blew up or went awry, we signal a condition.
    (unless (eq infer-style :rectified)
      (error "Unknown infer-style ~(~S~) for this instance: ~A"
             infer-style inst))
    t))

;;; ABOVE is rectify and associated functions
;;; ---------------------------------------------------------------------------
;;; BELOW is materialize, the entry point into the entire above
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
