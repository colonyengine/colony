(in-package #:%first-light)

(defclass actor (queryable)
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
         :initform 0)
   (%context :reader context
             :initarg :context)))

(defun make-actor (context &rest args &key &allow-other-keys)
  (let ((actor (apply #'make-instance 'actor
                      :context context
                      args)))
    (register-object-uuid actor)
    (register-object-id actor)
    actor))

(defun attach-component (actor component)
  (detach-component actor component)
  (enqueue-attach-event component actor)
  (setf (actor component) actor
        (u:href (components actor) component) component)
  (let* ((core (core (context actor)))
         (qualified-type (qualify-component core (component-type component))))
    (push component (u:href (components-by-type actor) qualified-type))))

(defun attach-multiple-components (actor &rest components)
  (dolist (component components)
    (attach-component actor component)))

;; TODO: This function is going to be hard to implement with the type tables.
;; just a set of nested hash tables. It might force us to move the type table to
;; a real object, so it can keep track of what is stored inside of it.
(defun number-of-components (actor)
  (hash-table-count (components actor)))

(defun detach-component (actor component)
  "If COMPONENT is contained in the ACTOR. Remove it. Otherwise, do nothing."
  (when (remhash component (components actor))
    (symbol-macrolet ((typed-components (u:href (components-by-type actor)
                                                (component-type component))))
      (enqueue-detach-event component actor)
      (setf (actor component) nil)
      (setf typed-components
            (remove-if (lambda (x) (eq x component)) typed-components)))))

(defun actor-components-by-type (actor component-type)
  "Get a list of all components of type COMPONENT-TYPE for the given ACTOR."
  (let* ((core (core (context actor)))
         (qualified-type (qualify-component core component-type)))
    (u:href (components-by-type actor) qualified-type)))

(defun actor-component-by-type (actor component-type)
  "Get the first component of type COMPONENT-TYPE for the given ACTOR.
Returns the rest of the components as a secondary value if there are more than
one of the same type."
  (let* ((core (core (context actor)))
         (qualified-type (qualify-component core component-type))
         (components (actor-components-by-type actor qualified-type)))
    (values (first components)
            (rest components))))

(defun spawn-actor (actor &key (parent :universe))
  "Take the ACTOR and place into the initializing db's and view's in the CORE.
The actor is not yet in the scene and the main loop protocol will not be called
on it or its components. If keyword argument :PARENT is supplied it is an actor
reference which will be the parent of the spawning actor. It defaults to
:universe, which means make this actor a child of the universe actor."
  (let ((core (core (context actor)))
        (actor-transform (actor-component-by-type actor 'fl.comp:transform)))
    (cond
      ((eq parent :universe)
       (unless (fl.comp:parent actor-transform)
         (fl.comp:transform-add-child
          (actor-component-by-type (scene-tree core) 'fl.comp:transform)
          (actor-component-by-type actor 'fl.comp:transform))))
      ((typep parent 'actor)
       (fl.comp:transform-add-child
        (actor-component-by-type parent 'fl.comp:transform)
        (actor-component-by-type actor 'fl.comp:transform)))
      ((null parent)
       (u:noop))
      (t
       (error "Cannot parent actor ~s to unknown parent ~s" actor parent)))
    (setf (u:href (actor-preinit-db (tables core)) actor) actor)
    (u:do-hash-values (v (components actor))
      (setf (type-table (canonicalize-component-type (component-type v)
                                                     core)
                        (component-preinit-by-type-view (tables core)))
            v))))

(defun actor/preinit->init (actor)
  (let ((core (core (context actor))))
    (remhash actor (actor-preinit-db (tables core)))
    (setf (u:href (actor-init-db (tables core)) actor) actor)))

(defun actor/init->active (actor)
  (let ((core (core (context actor))))
    (remhash actor (actor-init-db (tables core)))
    (setf (state actor) :active
          (u:href (actor-active-db (tables core)) actor) actor)))

(defmethod destroy-after-time ((thing actor) &key (ttl 0))
  (let* ((core (core (context thing)))
         (table (actor-predestroy-view (tables core))))
    (when (eq thing (scene-tree core))
      (error "Cannot destroy the scene tree root."))
    (setf (ttl thing) (and ttl (max 0 ttl)))
    (if ttl
        (setf (u:href table thing) thing)
        ;; If the TTL is stopped, we want to remove the actor from the
        ;; pre-destroy view!
        (remhash thing table))))

(defun actor/init-or-active->destroy (actor)
  ;; TODO: A different logic error (that of a destroyed object not having its
  ;; components also destroyed), and a destroyed object being destroyed
  ;; twice--which is legal but not explicitly handled) caused this UNLESS to be
  ;; here. Replace with new flow.
  (unless (eq (state actor) :destroy)
    (let* ((core (core (context actor)))
           (tables (tables core)))
      (unless (plusp (ttl actor))
        (setf (u:href (actor-destroy-db tables) actor) actor
              (state actor) :destroy)
        (remhash actor (actor-predestroy-view tables))
        (unless (remhash actor (actor-active-db tables))
          (remhash actor (actor-preinit-db tables)))
        (u:do-hash-values (v (components actor))
          (setf (ttl v) 0)
          (enqueue-detach-event v actor)
          (component/init-or-active->destroy v))))))

(defun actor/destroy-descendants (actor)
  (flet ((destroy-actor (actor)
           (setf (ttl actor) 0)
           (actor/init-or-active->destroy actor)
           (deregister-object-uuid actor)
           (deregister-object-id actor)))
    (when actor
      (fl.comp:map-nodes
       (lambda (x) (destroy-actor (actor x)))
       (actor-component-by-type actor 'fl.comp:transform)))))

(defun actor/disconnect (actor)
  (when (eq (id actor) 'universe)
    (error "Cannot disconnect the top-level universe node."))
  (let ((actor-transform (actor-component-by-type actor 'fl.comp:transform)))
    (fl.comp:transform-remove-child (fl.comp:parent actor-transform)
                                    actor-transform)))

(defun actor/destroy->released (actor)
  (let ((core (core (context actor))))
    (unless (zerop (number-of-components actor))
      (error "actor/destroy->released: destroyed actor still has components!"))
    (remhash actor (actor-destroy-db (tables core)))))

(defun actor/countdown-to-destruction (actor)
  (when (plusp (ttl actor))
    (decf (ttl actor) (frame-time (context actor)))))
