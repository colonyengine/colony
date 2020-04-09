(in-package #:virality)

(defclass actor (kernel)
  ((%components :reader components
                :initform (u:dict))
   (%components-by-type :reader %components-by-type
                        :initform (u:dict))
   (%prefab-node :reader prefab-node
                 :initarg :prefab-node
                 :initform nil)))

(defun make-actor (context &rest args &key &allow-other-keys)
  (let ((actor (apply #'make-instance 'actor :context context args)))
    (register-kernel-uuid actor)
    (register-kernel-id actor)
    actor))

(defun spawn-actor (actor &key (parent :universe))
  "Take the ACTOR and place into the initializing db's and view's in the CORE.
The actor is not yet in the scene and the main loop protocol will not be called
on it or its components. If keyword argument :PARENT is supplied it is an actor
reference which will be the parent of the spawning actor. It defaults to
:universe, which means make this actor a child of the universe actor."
  (let* ((core (core actor))
         (tables (tables core))
         (transform (component-by-type actor 'c/xform:transform)))
    (cond
      ((eq parent :universe)
       (unless (c/xform::parent transform)
         (c/xform:add-child
          (component-by-type (scene-tree core) 'c/xform:transform)
          (component-by-type actor 'c/xform:transform))))
      ((typep parent 'actor)
       (c/xform:add-child
        (component-by-type parent 'c/xform:transform)
        (component-by-type actor 'c/xform:transform)))
      ((null parent)
       (u:noop))
      (t
       (error "Cannot parent actor ~s to unknown parent ~s" actor parent)))
    (setf (u:href (actor-preinit-db tables) actor) actor)
    (u:do-hash-values (v (components actor))
      (setf (type-table (canonicalize-component-type (component-type v) core)
                        (component-preinit-by-type-view tables))
            v))))

(defun actor/preinit->init (actor)
  (let ((tables (tables (core actor))))
    (remhash actor (actor-preinit-db tables))
    (setf (u:href (actor-init-db tables) actor) actor)))

(defun actor/init->active (actor)
  (let ((tables (tables (core actor))))
    (remhash actor (actor-init-db tables))
    (setf (state actor) :active
          (u:href (actor-active-db tables) actor) actor)))

(defun actor/init-or-active->destroy (actor)
  ;; TODO: A different logic error (that of a destroyed object not having its
  ;; components also destroyed), and a destroyed object being destroyed
  ;; twice--which is legal but not explicitly handled) caused this UNLESS to be
  ;; here. Replace with new flow.
  (unless (or (eq (state actor) :destroy)
              (plusp (ttl actor)))
    (let ((tables (tables (core actor))))
      (setf (u:href (actor-destroy-db tables) actor) actor
            (state actor) :destroy)
      (remhash actor (actor-predestroy-view tables))
      (unless (remhash actor (actor-active-db tables))
        (remhash actor (actor-preinit-db tables)))
      (u:do-hash-values (v (components actor))
        (setf (ttl v) 0)
        (enqueue-detach-event v actor)
        (component/init-or-active->destroy v)))))

(defun actor/destroy-descendants (actor)
  (flet ((destroy-actor (actor)
           (setf (ttl actor) 0)
           (actor/init-or-active->destroy actor)
           (deregister-kernel-uuid actor)
           (deregister-kernel-id actor)))
    (when actor
      (c/xform::map-nodes
       (lambda (x) (destroy-actor (actor x)))
       (component-by-type actor 'c/xform:transform)))))

(defun actor/disconnect (actor)
  (when (eq (id actor) 'universe)
    (error "Cannot disconnect the top-level universe node."))
  (let ((transform (component-by-type actor 'c/xform:transform)))
    (c/xform:remove-child (c/xform::parent transform) transform)))

(defun actor/destroy->released (actor)
  (let ((core (core actor)))
    (unless (zerop (hash-table-count (components actor)))
      (error "Destroyed actor still has components."))
    (remhash actor (actor-destroy-db (tables core)))))

(defun actor/countdown-to-destruction (actor)
  (when (plusp (ttl actor))
    (decf (ttl actor) (frame-time (context actor)))))
