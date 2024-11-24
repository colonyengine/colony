(in-package #:colony.texture-map)


;; NOTES:
;;
;; 1. DEFINE-TEXTURE-MAP defines a "metaspace texture-map descriptor" in the
;;    global metaspace when the form is evaluated at macro time.
;;
;; 2. At engine start, the "metaspace texture-map descriptor" is "reified" (via
;;    deep copy) into a "semantic texture-map descriptor". This is a (usually
;;    unchanged) clone of the metaspace descriptor into a running core
;;    instance's data structure tables.
;;
;; 3. The "semantic texture-map descriptor" is "resolved" into one or more (in
;;    the case of cube maps) "resolved texture-maps". A "resolved texture-map"
;;    is directly visible and manipulable to the appdev, app, textures, or
;;    other uses of the texture-map.
;;
;; 4. A "resolved texture-map's" assets may be, partially or fully,
;;    "materialized" into main memory in an eager or lazy basis.
;;
;; 5. The "materialized" data might persist in main memory and possibly be
;;    uploaded to the gpu or copied from the gpu to main memory.
;;
;; 6. The "materialized" data may be modified repeatedly and repeatedly
;;    uploaded to the gpu or downloaded from the gpu. Materialized data, unless
;;    the texture-map marks it persistant, may be "dematerialized" and then
;;    later "rematerialized" if needed.
;;
;; Constraints:
;;
;; - Any time the actual texture-map data is accessed and it is not
;;   materialized, it will IMMEDIATELY materialize. Otherwise it will lazily
;;   materialize (and possibly concurrently with other materializations).


;; TODO: Fix this below to figure out what to do with the extra
;; texture-map instances if they are present. We need to figure
;; out how, or when, they should get put into the metaspace or if
;; it should be put there at all.
(defun update-texture-map (context old-descriptor new-descriptor)
  (declare (ignore context old-descriptor new-descriptor))
  (error "update-texture-map: Implement me! Figure out how to replace a texture-map at runtime during live coding.")

  ;; COmment this in and finish it.
  #++(tpool:push-queue
      (c::thread-pool (c::core context))
      :recompile
      (list
       :texture-map
       (lambda (core)
         (error "update-texture-map: Implement me!")
         ))))

(defun update-texture-map/interactively (old-descriptor new-descriptor)
  (c:with-selected-interactive-core (core)
    (update-texture-map (c:context core) old-descriptor new-descriptor)))

;;
;; Old implementation, convert over to the new implementation.
;;
;; NIL is a reserved name which means "anonymous texture-map".
;; Return three values:
;; First: the name of the texture (even if assigned due to it being anonymous).
;; Second: The tmap AST instance for this texture map.
;; Third: If there were additional texture-maps defined, like say faces of a
;;        cube map, then return all the ASTs for those additional texture maps
;;        in a list.
(defmacro define-texture-map-original (name data-model &body body)
  (u:with-gensyms (tmap extra-tmaps-list context anonymous-p canon-name
                        desc-lookup old-desc new-desc)
    (u:mvlet ((model style store (parse-data-model data-model)))
      ;; TODO
      ;; Make parse-texture-map returns two values: The first value is the
      ;; texture map AST instance described by the define-texture-map form.
      ;; The second value is a list of any additional texture-map AST instances
      ;; that were created as a result of processing this form.  In the case of
      ;; non-cube texture-maps, there will be only the first value and a nil
      ;; second value, but a cube map *may* return 6 additional texture maps
      ;; that might need to go into the meta storage!
      `(multiple-value-bind (,tmap ,extra-tmaps-list)
           ,(parse-texture-map name model style (car store) body)

         (u:comment "Attempt to put this texture-map into the right place.")
         (with-accessors ((,anonymous-p texmap::anonymous-p)
                          (,canon-name texmap::name)) ,tmap
           (let ((,context (abag:sattr ,tmap :context)))
             ;; Figure out if we can automatically book keep this texture-map
             ;; (and any extra-tmaps) under the conditions of its creation.  We
             ;; check in a particular order for performance reasons.
             (cond
               ;; CASE 0
               ;; This means we likely expanded into runtime code and we're
               ;; making an anonymous texture-map which we'll poke into the
               ;; context.
               ((and ,context ,anonymous-p)
                ;; TODO: Reify texture-map AST(s) into CORE. Implement me very
                ;; soon.
                (error "define-texture-map: Handle case 0"))

               ;; CASE 1
               ;; We assign a meaning of just return the parsed AST to the
               ;; appdev and do no other work with this texture-map. It is up
               ;; to the appdev to reify or otherwise insert this texture-map
               ;; (and associated extra-maps if any) into CORE if they desire
               ;; to do so.
               ((and (not ,context) ,anonymous-p)
                T)

               ;; CASE 2
               ;; We don't have a meaning for this situation yet.
               ((and ,context (not ,anonymous-p))
                (error "define-texture-map: Handle case 2."))

               ;; CASE 3
               ;; This means we generally evaluated at toplevel during the load
               ;; of the system, or are in a live-coding situation, or it was
               ;; typed into the REPL.  So insert it into the metaspace and/or
               ;; update interactively if possible.
               ((and (not ,context) (not ,anonymous-p))
                (symbol-macrolet ((,desc-lookup (u:href c::=meta/texture-maps=
                                                        ,canon-name)))
                  (let ((,new-desc (make-texture-map-descriptor
                                    ,canon-name ,tmap ,extra-tmaps-list
                                    `(define-texture-map ,',name ,',data-model
                                       ,@',body)))
                        (,old-desc ,desc-lookup))
                    (setf ,desc-lookup ,new-desc)
                    (update-texture-map/interactively ,old-desc ,new-desc)))))

             (values ,canon-name ,tmap ,extra-tmaps-list)))))))

;; Soon to be new stuff.
(defmacro define-texture-map-new (name data-model &body body)
  (u:mvlet* ((model style store (unpack-data-model data-model))
             (texmap-form
              anonymous-p (parse-texture-map name model style store body)))
    ;; TODO: testing. make a texture-map-descriptor here, etc, etc.
    `(progn ',anonymous-p
            ',texmap-form)))

;; All current forms spread through the examples and such are screwed and
;; so this just removes them while I work on it.
(defmacro define-texture-map (name data-model &body body)
  nil)
