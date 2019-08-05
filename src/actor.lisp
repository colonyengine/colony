(in-package #:virality.actors)

(defclass actor (v::queryable)
  ((%state :accessor state
           :initarg :state
           :initform :initialize)
   (%components :reader components
                :initform (u:dict))
   (%components-by-type :reader components-by-type
                        :initform (u:dict))
   (%prefab-node :reader prefab-node
                 :initarg :prefab-node
                 :initform nil)
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)))

(defun make-actor (context &rest args &key &allow-other-keys)
  (let ((actor (apply #'make-instance 'actor :context context args)))
    (v::register-object-uuid actor)
    (v::register-object-id actor)
    actor))

(defun spawn-actor (actor &key (parent :universe))
  "Take the ACTOR and place into the initializing db's and view's in the CORE.
The actor is not yet in the scene and the main loop protocol will not be called
on it or its components. If keyword argument :PARENT is supplied it is an actor
reference which will be the parent of the spawning actor. It defaults to
:universe, which means make this actor a child of the universe actor."
  (let ((core (v::core (v:context actor)))
        (transform (v:component-by-type actor 'comp.transform:transform)))
    (cond
      ((eq parent :universe)
       (unless (comp.transform::parent transform)
         (comp.transform:transform-add-child
          (v:component-by-type (v::scene-tree core) 'comp.transform:transform)
          (v:component-by-type actor 'comp.transform:transform))))
      ((typep parent 'actor)
       (comp.transform:transform-add-child
        (v:component-by-type parent 'comp.transform:transform)
        (v:component-by-type actor 'comp.transform:transform)))
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
  (let ((tables (v::tables (v::core (v:context actor)))))
    (remhash actor (v::actor-preinit-db tables))
    (setf (u:href (v::actor-init-db tables) actor) actor)))

(defun init->active (actor)
  (let ((tables (v::tables (v::core (v:context actor)))))
    (remhash actor (v::actor-init-db tables))
    (setf (state actor) :active
          (u:href (v::actor-active-db tables) actor) actor)))

(defmethod v:destroy-after-time ((thing actor) &key (ttl 0))
  (let* ((core (v::core (v:context thing)))
         (table (v::actor-predestroy-view (v::tables core))))
    (when (eq thing (v::scene-tree core))
      (error "Cannot destroy the scene tree root."))
    ;; TODO: this needs fixing because TTL is never nil
    (setf (ttl thing) (and ttl (max 0 ttl)))
    ;; TODO: Same for this
    (if ttl
        (setf (u:href table thing) thing)
        ;; If the TTL is stopped, we want to remove the actor from the
        ;; pre-destroy view!
        (remhash thing table))))

(defun init-or-active->destroy (actor)
  ;; TODO: A different logic error (that of a destroyed object not having its
  ;; components also destroyed), and a destroyed object being destroyed
  ;; twice--which is legal but not explicitly handled) caused this UNLESS to be
  ;; here. Replace with new flow.
  (unless (eq (state actor) :destroy)
    (let* ((core (v::core (v:context actor)))
           (tables (v::tables core)))
      (unless (plusp (ttl actor))
        (setf (u:href (v::actor-destroy-db tables) actor) actor
              (state actor) :destroy)
        (remhash actor (v::actor-predestroy-view tables))
        (unless (remhash actor (v::actor-active-db tables))
          (remhash actor (v::actor-preinit-db tables)))
        (u:do-hash-values (v (components actor))
          (setf (v::ttl v) 0)
          (v::enqueue-detach-event v actor)
          (v::component/init-or-active->destroy v))))))

(defun destroy-descendants (actor)
  (flet ((destroy-actor (actor)
           (setf (ttl actor) 0)
           (init-or-active->destroy actor)
           (v::deregister-object-uuid actor)
           (v::deregister-object-id actor)))
    (when actor
      (comp.transform::map-nodes
       (lambda (x) (destroy-actor (actor x)))
       (v:component-by-type actor 'comp.transform:transform)))))

(defun disconnect (actor)
  (when (eq (v:id actor) 'universe)
    (error "Cannot disconnect the top-level universe node."))
  (let ((actor-transform (v:component-by-type actor 'comp.transform:transform)))
    (comp.transform:transform-remove-child (comp.transform::parent actor-transform)
                                           actor-transform)))

(defun destroy->released (actor)
  (let ((core (v::core (v:context actor))))
    (unless (zerop (hash-table-count (components actor)))
      (error "Destroyed actor still has components."))
    (remhash actor (v::actor-destroy-db (v::tables core)))))

(defun countdown-to-destruction (actor)
  (when (plusp (ttl actor))
    (decf (ttl actor) (v:frame-time (v:context actor)))))
