(in-package :fl.core)

(defclass actor ()
  ((%id :reader id
        :initarg :id)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%components :reader components
                :initform (make-hash-table))
   (%components-by-type :reader components-by-type
                        :initform (make-hash-table))
   (%scene :accessor scene
           :initarg :scene)
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)
   (%core-state :reader core-state
                :initarg :core-state)))

(defmethod print-object ((object actor) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (id object))))

(defun make-actor (context &rest args)
  (apply #'make-instance 'actor :core-state (core-state context) args))

(defun attach-component (actor component)
  (unless (actor component)
    (setf (actor component) actor))
  (setf (gethash component (components actor)) component)
  (push component (gethash (component-type component)
                           (components-by-type actor))))

(defun attach-multiple-components (actor components)
  (dolist (component components)
    (attach-component actor component)))

(defun number-of-components (actor)
  (hash-table-count (components actor)))

(defun detach-component (actor component)
  "If COMPONENT is contained in the ACTOR. Remove it. Otherwise, do nothing."
  (when (remhash component (components actor))
    (symbol-macrolet ((typed-components (gethash (component-type component)
                                                 (components-by-type actor))))
      (setf typed-components
            (remove-if
             (lambda (c) (eq c component))
             typed-components)))))

(defun actor-components-by-type (actor component-type)
  "Get a list of all components of type COMPONENT-TYPE for the given ACTOR."
  (gethash component-type (components-by-type actor)))

(defun actor-component-by-type (actor component-type)
  "Get the first component of type COMPONENT-TYPE for the given ACTOR.
Returns T as a secondary value if there exists more than one component of that type."
  (let* ((qualified-type (qualify-component (core-state actor) component-type))
         (components (actor-components-by-type actor qualified-type)))
    (values (first components)
            (> (length components) 1))))

;; TODO: This uses ensure-symbol a lot because it wants to know about the
;; fl.comp.transform:transform component. However, that package is not available
;; at read time for this code. Think about a better way, if any, to do this
;; operation.
(defun spawn-actor (actor context &key (parent :universe))
  "Take the ACTOR and place into the initializing db's and view's in the CORE-STATE. The actor is
not yet in the scene and the main loop protocol will not be called on it or its components. If
keyword argument :PARENT is supplied it is an actor reference which will be the parent of the
spawning actor. It defaults to :universe, which means make this actor a child of the universe
actor."
  (let* ((core-state (core-state context))
         (sym/transform (ensure-symbol 'transform 'fl.comp.transform))
         (sym/add-child-func (ensure-symbol 'add-child 'fl.comp.transform))
         (sym/parent-func (ensure-symbol 'parent 'fl.comp.transform))
         (actor-transform (actor-component-by-type actor sym/transform)))
    (cond
      ((eq parent :universe)
       ;; TODO: This isn't exactly correct, but will work in most cases. Namely,
       ;; it works in the scene DSL expansion since we add children before
       ;; spawning the actors. We may be able to fix the scene dsl expansion to
       ;; just supply the :parent keyword to spawn-actor instead and forgo the
       ;; add-child calls there. Usually, when a user calls SPAWN-ACTOR in their
       ;; code, they will either leave :parent at default, or already have an
       ;; actor to reference as the parent.
       (unless (funcall sym/parent-func actor-transform)
         (funcall sym/add-child-func
                  (actor-component-by-type (scene-tree core-state) sym/transform)
                  (actor-component-by-type actor sym/transform))))
      ((typep parent 'actor)
       (funcall sym/add-child-func
                (actor-component-by-type parent sym/transform)
                (actor-component-by-type actor sym/transform)))
      ((null parent)
       ;; NOTE: We're in %make-scene-tree, do nothing since we're making the
       ;; universe!
       nil)
      (t
       (error "Cannot parent actor ~A to unknown parent ~A" actor parent)))
    (setf (gethash actor (actor-preinit-db (tables core-state))) actor)
    (maphash
     (lambda (k v)
       (declare (ignore k))
       (setf (type-table (canonicalize-component-type (component-type v) core-state)
                         (component-preinit-by-type-view (tables core-state)))
             v))
     (components actor))))

(defun actor/preinit->init (core-state actor)
  (remhash actor (actor-preinit-db (tables core-state)))
  (setf (gethash actor (actor-init-db (tables core-state))) actor))

(defun actor/init->active (core-state actor)
  (remhash actor (actor-init-db (tables core-state)))
  (setf (state actor) :active
        (gethash actor (actor-active-db (tables core-state))) actor))

(defmethod destroy ((thing actor) (context context) &key (ttl 0))
  (let ((core-state (core-state context)))
    (setf (ttl thing) (if (minusp ttl) 0 ttl)
          (gethash thing (actor-predestroy-view (tables core-state))) thing)))

(defun actor/init-or-active->destroy (core-state actor)
  (setf (gethash actor (actor-destroy-db (tables core-state))) actor
        (state actor) :destroy)
  (remhash actor (actor-predestroy-view (tables core-state)))
  (unless (remhash actor (actor-active-db (tables core-state)))
    (remhash actor (actor-preinit-db (tables core-state))))
  (maphash
   (lambda (k component)
     (declare (ignore k))
     (setf (ttl component) 0)
     (component/init-or-active->destroy core-state component))
   (components actor)))

(defun actor/destroy-descendants (core-state actor)
  (let ((sym/transform (ensure-symbol 'transform 'fl.comp.transform))
        (sym/map-nodes (ensure-symbol 'map-nodes 'fl.comp.transform)))
    (flet ((destroy-actor (descendant-actor-transform)
             (let ((destroying-actor (actor descendant-actor-transform)))
               (setf (ttl destroying-actor) 0)
               (actor/init-or-active->destroy core-state destroying-actor))))
      (funcall sym/map-nodes
               #'destroy-actor
               (actor-component-by-type actor sym/transform)))))

;; TODO: this should probably never be run on the @universe actor. :)
(defun actor/disconnect (core-state actor)
  (declare (ignore core-state))
  (let* ((sym/transform (ensure-symbol 'transform 'fl.comp.transform))
         (sym/remove-child (ensure-symbol 'remove-child 'fl.comp.transform))
         (sym/parent (ensure-symbol 'parent 'fl.comp.transform))
         (actor-transform (actor-component-by-type actor sym/transform)))
    (funcall sym/remove-child (funcall sym/parent actor-transform) actor-transform)))

(defun actor/destroy->released (core-state actor)
  ;; At this point, the actor should be empty, we'll check it just in case.
  (unless (zerop (number-of-components actor))
    (error "actor/destroy->released: destroyed actor still has components!"))
  (remhash actor (actor-destroy-db (tables core-state))))

(defun actor/countdown-to-destruction (core-state actor)
  (when (plusp (ttl actor))
    (decf (ttl actor) (box.frame:frame-time (display core-state)))))
