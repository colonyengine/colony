(in-package :%first-light)

(defclass actor ()
  ((%id :reader id
        :initarg :id)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%components :reader components
                :initform (fl.util:dict #'eq))
   (%components-by-type :reader components-by-type
                        :initform (fl.util:dict #'eq))
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)
   (%context :reader context
             :initarg :context)))

(fl.util:define-printer (actor stream :type t)
  (format stream "~a" (id actor)))

(defun make-actor (context &rest args)
  (apply #'make-instance 'actor :context context args))

(defun attach-component (actor component)
  ;; First, we detach from the current actor (if applicable)
  (detach-component actor component)

  ;; Then, we attach to the new component.
  (enqueue-attach-event component actor)
  (setf (actor component) actor)
  (setf (fl.util:href (components actor) component) component)
  (let* ((core-state (core-state (context actor)))
         (qualified-type
           (qualify-component core-state (component-type component))))
    (push component
          (fl.util:href (components-by-type actor) qualified-type))))

(defun attach-multiple-components (actor &rest components)
  (dolist (component components)
    (attach-component actor component)))

;; TODO: This function is going to be hard to implement with the type tables.
;; just a set of nested hash tables. It might force us to move the type table
;; to a real object, so it can keep track of what is stored inside of it.
(defun number-of-components (actor)
  (hash-table-count (components actor)))

(defun detach-component (actor component)
  "If COMPONENT is contained in the ACTOR. Remove it. Otherwise, do nothing."
  (when (remhash component (components actor))
    (symbol-macrolet ((typed-components (fl.util:href (components-by-type actor)
                                                      (component-type component))))
      (enqueue-detach-event component actor)
      (setf (actor component) nil)
      (setf typed-components
            (remove-if (lambda (c) (eq c component)) typed-components)))))

(defun actor-components-by-type (actor component-type)
  "Get a list of all components of type COMPONENT-TYPE for the given ACTOR."
  (let* ((core-state (core-state (context actor)))
         (qualified-type (qualify-component core-state component-type)))
    (fl.util:href (components-by-type actor) qualified-type)))

(defun actor-component-by-type (actor component-type)
  "Get the first component of type COMPONENT-TYPE for the given ACTOR.
Returns the rest of the components as a secondary value if there are more than one of the same type."
  (let* ((core-state (core-state (context actor)))
         (qualified-type (qualify-component core-state component-type))
         (components (actor-components-by-type actor qualified-type)))
    (values (first components)
            (rest components))))

(defun spawn-actor (actor &key (parent :universe))
  "Take the ACTOR and place into the initializing db's and view's in the CORE-STATE. The actor is
not yet in the scene and the main loop protocol will not be called on it or its components. If
keyword argument :PARENT is supplied it is an actor reference which will be the parent of the
spawning actor. It defaults to :universe, which means make this actor a child of the universe
actor."
  (let ((core-state (core-state (context actor)))
        (actor-transform (actor-component-by-type actor 'fl.comp:transform)))
    (cond
      ((eq parent :universe)
       ;; TODO: This isn't exactly correct, but will work in most cases. Namely, it works in the
       ;; scene DSL expansion since we add children before spawning the actors. We may be able to
       ;; fix the scene dsl expansion to just supply the :parent keyword to spawn-actor instead and
       ;; forgo the transform-add-child calls there. Usually, when a user calls SPAWN-ACTOR in their
       ;; code, they will either leave :parent at default, or already have an actor to reference as
       ;; the parent.
       (unless (fl.comp::parent actor-transform)
         (fl.comp:transform-add-child (actor-component-by-type (scene-tree core-state)
                                                               'fl.comp:transform)
                                      (actor-component-by-type actor 'fl.comp:transform))))
      ((typep parent 'actor)
       (fl.comp:transform-add-child (actor-component-by-type parent 'fl.comp:transform)
                                    (actor-component-by-type actor 'fl.comp:transform)))
      ((null parent)
       (fl.util:noop))
      (t
       (error "Cannot parent actor ~s to unknown parent ~s" actor parent)))
    (setf (fl.util:href (actor-preinit-db (tables core-state)) actor) actor)
    (fl.util:do-hash-values (v (components actor))
      (setf (type-table (canonicalize-component-type (component-type v) core-state)
                        (component-preinit-by-type-view (tables core-state)))
            v))))

(defun actor/preinit->init (actor)
  (let ((core-state (core-state (context actor))))
    (remhash actor (actor-preinit-db (tables core-state)))
    (setf (fl.util:href (actor-init-db (tables core-state)) actor) actor)))

(defun actor/init->active (actor)
  (let ((core-state (core-state (context actor))))
    (remhash actor (actor-init-db (tables core-state)))
    (setf (state actor) :active
          (fl.util:href (actor-active-db (tables core-state)) actor) actor)))

(defmethod destroy ((thing actor) &key (ttl 0))
  (let ((core-state (core-state (context thing))))
    (setf (ttl thing) (if (minusp ttl) 0 ttl)
          (fl.util:href (actor-predestroy-view (tables core-state)) thing) thing)))

(defun actor/init-or-active->destroy (actor)
  (let* ((core-state (core-state (context actor)))
         (tables (tables core-state)))
    (unless (plusp (ttl actor))
      (setf (fl.util:href (actor-destroy-db tables) actor) actor
            (state actor) :destroy)
      (remhash actor (actor-predestroy-view tables))
      (unless (remhash actor (actor-active-db tables))
        (remhash actor (actor-preinit-db tables)))
      (fl.util:do-hash-values (v (components actor))
        (setf (ttl v) 0)
        (component/init-or-active->destroy v)))))

(defun actor/destroy-descendants (actor)
  (flet ((destroy-actor (descendant-actor-transform)
           (let ((destroying-actor (actor descendant-actor-transform)))
             (setf (ttl destroying-actor) 0)
             (actor/init-or-active->destroy destroying-actor))))
    (when actor
      (fl.comp::map-nodes #'destroy-actor (actor-component-by-type actor 'fl.comp:transform)))))

;; TODO: this should probably never be run on the @universe actor. :)
(defun actor/disconnect (actor)
  (let ((actor-transform (actor-component-by-type actor 'fl.comp:transform)))
    (fl.comp:transform-remove-child (fl.comp::parent actor-transform) actor-transform)))

(defun actor/destroy->released (actor)
  (let ((core-state (core-state (context actor))))
    (unless (zerop (number-of-components actor))
      (error "actor/destroy->released: destroyed actor still has components!"))
    (remhash actor (actor-destroy-db (tables core-state)))))

(defun actor/countdown-to-destruction (actor)
  (when (plusp (ttl actor))
    (decf (ttl actor) (frame-time (context actor)))))
