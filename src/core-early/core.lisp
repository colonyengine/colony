(in-package #:virality)

;;;; Some of the implementation of the datatype CORE.

(defun pending-preinit-tasks-p (core)
  "Return T if there are ANY components or actors in the preinit data structures
in CORE."
  (or (plusp (hash-table-count (actor-preinit-db (tables core))))
      (block done
        (u:do-hash-values (v (component-preinit-by-type-view (tables core)))
          (when (plusp (hash-table-count v))
            (return-from done t))))))

(defun pending-predestroy-tasks-p (core)
  "Return T if there are ANY components or actors that are in the predestroy
  data structures in CORE."
  (or (plusp (hash-table-count (component-predestroy-view (tables core))))
      (plusp (hash-table-count (actor-predestroy-view (tables core))))))

(defun pending-destroy-tasks-p (core)
  "Return T of there are ANY components or actors that are in the destroy data
structures in CORE."
  (or (plusp (hash-table-count (actor-destroy-db (tables core))))
      (block done
        (u:do-hash-values (v (component-destroy-by-type-view (tables core)))
          (when (plusp (hash-table-count v))
            (return-from done t))))))

(defun make-scene-tree (core)
  (let* ((context (context core))
         (actor (make-actor context :id 'universe :display-id "Universe"))
         (transform (make-component context 'comp:transform :actor actor)))
    (attach-component actor transform)
    (spawn-actor actor :parent nil)
    (execute-flow core :default
                  'initialize-phase
                  'entry/initialize-phase
                  :come-from-state-name 'ef-make-scene-tree)
    (setf (slot-value core '%scene-tree) actor)))

(defun make-core (config)
  ;; TODO: LOAD-CONFIG should have been executed before this. Write a small
  ;; piece of code that can check this fact right here.
  (let ((core
          (make-instance
           'core
           ;; NOTE: It is possible the ordering of this initialization may be
           ;; significant.
           ;;
           ;; NOTE: It is debatable that we should move these out of this
           ;; function and into the engine.lisp INITIALIZE function so we can
           ;; finely control the initialization of everything.
           :config config
           :tables (make-instance 'bookkeeping-tables)
           :materials (make-materials-table)
           :texture-maps (texmaptab:make-texture-map-table)
           :textures (textab:make-texture-table)
           :thread-pool (tpool:make-thread-pool (or =threads= =cpu-count=))
           :context (make-context)
           :clock (make-clock))))

    (setf (slot-value (context core) '%core) core
          *core-debug* core)
    core))
