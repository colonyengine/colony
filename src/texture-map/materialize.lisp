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
                   (format t " Texture-map: ~A, image-element: ~A, ct ~A~%"
                           (texmap:name (car (rc:opaque-data ct-inst)))
                           (logloc (cdr (rc:opaque-data ct-inst)))
                           ct-inst)

                   (dolist (event (reverse event-list))
                     (format t "  ~S~%" event)))))


;; image element state machine

(defmethod rc:reserve-caching-task ((caching-task caching-task/image-element)
                                    resource-cache-scheduler resource-cache)
  (let ((cache-item (rc:make-cache-item :opaque-data caching-task
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
        (rc:size cache-item) nil)

  (when (rc:value cache-item)
    ;; TODO: Figure out if I need to free this resource or just drop it like
    ;; I am doing here and let the GC get it if noone else has reference.
    (setf (rc:value cache-item) nil))

  (rc:record-event
   (rc:info caching-task) caching-task `(:recycled ,cache-item))

  (values :reserved caching-task))


(defmethod rc:compute-caching-task ((caching-task caching-task/image-element)
                                    resource-cache-scheduler)
  (let* ((context (colony::context (rc::core caching-task)))
         ;; 1. convert asset key to filepath
         (asset (first (rc:key caching-task)))
         ;; TODO: Store this somewhere (maybe in cache-item?), cause it
         ;; needs to go into the data-element's PHYSLOC field.
         (asset-path
           (c:with-asset-cache context :texture asset
             (c::resolve-path asset)))
         ;; 2. load filepath as generic image
         (img (img:load-image asset-path)))

    ;; 3. finally store into caching task.
    (setf (rc:value caching-task) img)

    (rc:record-event
     (rc:info caching-task) caching-task `(:computed ,img))

    (values :computed caching-task)))

(defmethod rc:synchronize-from-caching-task ((caching-task
                                              caching-task/image-element)
                                             resource-cache-scheduler)
  (let ((resource-cache (c::resource-cache (rc:core caching-task))))
    (lock:with-lock (resource-cache)
      (let ((cache-item (rc:lookup-caching-task caching-task
                                                resource-cache-scheduler
                                                resource-cache)))
        (unless cache-item
          (dump-info (rc:info caching-task))

          (error "ERROR: There should be a cache item for caching-task ~A with key-list ~A but there is not!"
                 caching-task
                 (rc:key caching-task)))

        (let ((img (rc:value caching-task)))
          (setf (rc:value cache-item) img
                (rc:state cache-item) :cached
                ;; TODO: add size computation to image class., this is
                ;; wrong since it doesn't take into consideraton the
                ;; size of a pixel.
                (rc:size cache-item) (* (img:width img)
                                        (img:height img)))))

      (rc:record-event
       (rc:info caching-task) caching-task :synchronize-from)

      (values :synchronized caching-task))))

(defmethod rc:synchronize-to-caching-task ((caching-task
                                            caching-task/image-element)
                                           cache-item
                                           resource-cache-scheduler)
  (format t "synchronize-to-caching-task: not yet implemented: ~A~%"
          (type-of caching-task))

  (rc:record-event
   (rc:info caching-task) caching-task :synchronize-to)

  (values :synchronized caching-task))

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

    (format t "materialize: materializable texture-map instances (names): ~A~%"
            (mapcar #'texmap:name materializable-texmap-insts))
    (format t "materialize: unregistered texture-map names: ~A~%"
            unregistered-texmap-names)

    (values materializable-texmap-insts
            unregistered-texmap-names)))


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
      ;; Phase 1: load all data elements.
      (materialize-data-elements core texmap-names :force force)

    (declare (ignore materialized-texmap-insts unregistered-texmap-names))
    ;; Phase 2: Validate and fill in missing data in the in memory texture-map
    ;; now that we have the elements we need.

    ))
