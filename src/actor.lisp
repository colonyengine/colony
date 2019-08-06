(in-package #:virality.actors)

(defclass actor (v::kernel)
  ((%components :reader components
                :initform (u:dict))
   (%components-by-type :reader components-by-type
                        :initform (u:dict))
   (%prefab-node :reader prefab-node
                 :initarg :prefab-node
                 :initform nil)))

(defun make-actor (context &rest args &key &allow-other-keys)
  (let ((actor (apply #'make-instance 'actor :context context args)))
    (v::register-kernel-uuid actor)
    (v::register-kernel-id actor)
    actor))

(defun spawn-actor (actor &key (parent :universe))
  "Take the ACTOR and place into the initializing db's and view's in the CORE.
The actor is not yet in the scene and the main loop protocol will not be called
on it or its components. If keyword argument :PARENT is supplied it is an actor
reference which will be the parent of the spawning actor. It defaults to
:universe, which means make this actor a child of the universe actor."
  (let ((core (v::core actor))
        (transform (v:component-by-type actor 'c/xform:transform)))
    (cond
      ((eq parent :universe)
       (unless (c/xform::parent transform)
         (c/xform:transform-add-child
          (v:component-by-type (v::scene-tree core) 'c/xform:transform)
          (v:component-by-type actor 'c/xform:transform))))
      ((typep parent 'actor)
       (c/xform:transform-add-child
        (v:component-by-type parent 'c/xform:transform)
        (v:component-by-type actor 'c/xform:transform)))
      ((null parent)
       (u:noop))
      (t
       (error "Cannot parent actor ~s to unknown parent ~s" actor parent)))
    (setf (u:href (v::actor-preinit-db (v::tables core)) actor) actor)
    (u:do-hash-values (v (components actor))
      (setf (v::type-table (v::canonicalize-component-type
                            (v::component-type v) core)
                           (v::component-preinit-by-type-view (v::tables core)))
            v))))

(defun preinit->init (actor)
  (let ((tables (v::tables (v::core actor))))
    (remhash actor (v::actor-preinit-db tables))
    (setf (u:href (v::actor-init-db tables) actor) actor)))

(defun init->active (actor)
  (let ((tables (v::tables (v::core actor))))
    (remhash actor (v::actor-init-db tables))
    (setf (v::state actor) :active
          (u:href (v::actor-active-db tables) actor) actor)))

(defmethod v:destroy-after-time ((kernel actor) &key (ttl 0))
  (let* ((core (v::core kernel))
         (table (v::actor-predestroy-view (v::tables core))))
    (when (eq kernel (v::scene-tree core))
      (error "Cannot destroy the scene tree root."))
    ;; TODO: this needs fixing because TTL is never nil
    (setf (v::ttl kernel) (and ttl (max 0 ttl)))
    ;; TODO: Same for this
    (if ttl
        (setf (u:href table kernel) kernel)
        ;; If the TTL is stopped, we want to remove the actor from the
        ;; pre-destroy view!
        (remhash kernel table))))

(defun init-or-active->destroy (actor)
  ;; TODO: A different logic error (that of a destroyed object not having its
  ;; components also destroyed), and a destroyed object being destroyed
  ;; twice--which is legal but not explicitly handled) caused this UNLESS to be
  ;; here. Replace with new flow.
  (unless (eq (v::state actor) :destroy)
    (let* ((core (v::core actor))
           (tables (v::tables core)))
      (unless (plusp (v::ttl actor))
        (setf (u:href (v::actor-destroy-db tables) actor) actor
              (v::state actor) :destroy)
        (remhash actor (v::actor-predestroy-view tables))
        (unless (remhash actor (v::actor-active-db tables))
          (remhash actor (v::actor-preinit-db tables)))
        (u:do-hash-values (v (components actor))
          (setf (v::ttl v) 0)
          (v::enqueue-detach-event v actor)
          (v::component/init-or-active->destroy v))))))

(defun destroy-descendants (actor)
  (flet ((destroy-actor (actor)
           (setf (v::ttl actor) 0)
           (init-or-active->destroy actor)
           (v::deregister-kernel-uuid actor)
           (v::deregister-kernel-id actor)))
    (when actor
      (c/xform::map-nodes
       (lambda (x) (destroy-actor (actor x)))
       (v:component-by-type actor 'c/xform:transform)))))

(defun disconnect (actor)
  (when (eq (v:id actor) 'universe)
    (error "Cannot disconnect the top-level universe node."))
  (let ((actor-transform (v:component-by-type actor 'c/xform:transform)))
    (c/xform:transform-remove-child (c/xform::parent actor-transform)
                                    actor-transform)))

(defun destroy->released (actor)
  (let ((core (v::core actor)))
    (unless (zerop (hash-table-count (components actor)))
      (error "Destroyed actor still has components."))
    (remhash actor (v::actor-destroy-db (v::tables core)))))

(defun countdown-to-destruction (actor)
  (when (plusp (v::ttl actor))
    (decf (v::ttl actor) (v:frame-time (v:context actor)))))
