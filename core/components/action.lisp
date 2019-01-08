(in-package :first-light.components)

(define-component action ()
  ((name :default nil)
   (render :default nil)
   (elapsed :default 0)
   (duration :default 1)
   (finished-p :default nil)
   (self-finishing-p :default nil)
   (blocking-p :default nil)
   (cycle-p :default nil)
   (shape :default 'flm:linear)
   (attrs :default nil)
   (foo :default 0)))

(defun insert-action (action where &key replace-p target)
  (with-accessors ((name name) (actor actor)) action
    (let ((actions (actions (actor-component-by-type actor 'action-list))))
      (fl.dst:insert-dlist-node where actions name action :target-key target)
      (when replace-p
        (fl.dst:remove-dlist-nodes actions name))
      (on-action-insert action name)
      action)))

(defun remove-action (action)
  (with-accessors ((name name) (actor actor)) action
    (let ((actions (actions (actor-component-by-type actor 'action-list))))
      (fl.dst:remove-dlist-node actions name))))

(defun replace-action (action name &rest args)
  (apply #'reinitialize-instance action :name name :elapsed 0 :finished-p nil args))

(defun action-step (action)
  (with-accessors ((shape shape) (elapsed elapsed) (duration duration)) action
    (funcall shape (fl.util:clamp (/ elapsed duration) 0f0 1f0))))

;;; Action event hooks

(defgeneric on-action-insert (action name)
  (:method (action name)))

(defgeneric on-action-finish (action name)
  (:method (action name))
  (:method :around (action name)
    (with-accessors ((actor actor)) action
      (call-next-method)
      (v:trace :fl.comp.action "Action ~a finished for actor ~a." name (id actor)))))

(defgeneric on-action-update (action name)
  (:method (action name))
  (:method :before (action name)
    (with-accessors ((context context) (elapsed elapsed) (self-finishing-p self-finishing-p)
                     (duration duration) (finished-p finished-p))
        action
      (incf elapsed (frame-time context))
      (when (and (not self-finishing-p)
                 (>= elapsed duration))
        (setf finished-p t)))))

;;; Component event hooks

(defmethod on-component-initialize ((self action))
  ;; TODO: Once FL fixes procedural components, we can ensure an action-list component is added to
  ;; an actor here instead of explicitly adding it in the scene DSL.
  (with-accessors ((context context) (actor actor) (render render) (attrs attrs)) self
    (setf render (actor-component-by-type actor 'render)
          attrs (fl.util:plist->hash attrs :test #'eq))
    (unless (actor-component-by-type actor 'action-list)
      (attach-component actor (make-component context 'action-list)))
    (insert-action self :tail)))

;;; Built-in actions

;; sprite-animate

(defmethod on-action-update (action (name (eql 'sprite-animate)))
  (fl.util:when-let ((sprite (actor-component-by-type (actor action) 'sprite)))
    (with-accessors ((index index) (initial-index initial-index) (frames frames)) sprite
      (setf index (floor (fl.util:map-domain 0
                                             1
                                             initial-index
                                             (1- (+ initial-index frames))
                                             (action-step action)))))))

(defmethod on-action-finish (action (name (eql 'sprite-animate)))
  (when (cycle-p action)
    (replace-action action 'sprite-animate)))

;; fade

(defmethod on-action-update (action (name (eql 'fade-in)))
  (let ((material (material (render action))))
    (setf (mat-uniform-ref material :opacity) (action-step action))))

(defmethod on-action-finish (action (name (eql 'fade-in)))
  (when (cycle-p action)
    (replace-action action 'fade-out)))

(defmethod on-action-update (action (name (eql 'fade-out)))
  (let ((material (material (render action))))
    (setf (mat-uniform-ref material :opacity) (- 1 (action-step action)))))

(defmethod on-action-finish (action (name (eql 'fade-out)))
  (when (cycle-p action)
    (replace-action action 'fade-in)))

;; rotate

(defmethod on-action-update (action (name (eql 'rotate)))
  (let* ((transform (transform (render action)))
         (attrs (attrs action))
         (angle (or (fl.util:href attrs :angle) (* pi 2)))
         (step (fl.util:map-domain 0 1 0 angle (action-step action))))
    (ecase (or (fl.util:href attrs :axis) :z)
      (:x (rotate transform (flm:vec3 step 0 0) :replace-p t))
      (:y (rotate transform (flm:vec3 0 step 0) :replace-p t))
      (:z (rotate transform (flm:vec3 0 0 step) :replace-p t)))))

(defmethod on-action-finish (action (name (eql 'rotate)))
  (when (cycle-p action)
    (replace-action action 'rotate/reverse)))

(defmethod on-action-update (action (name (eql 'rotate/reverse)))
  (let* ((transform (transform (render action)))
         (attrs (attrs action))
         (angle (or (fl.util:href attrs :angle) (* pi 2)))
         (step (- angle (fl.util:map-domain 0 1 0 angle (action-step action)))))
    (ecase (or (fl.util:href attrs :axis) :z)
      (:x (rotate transform (flm:vec3 step 0 0) :replace-p t))
      (:y (rotate transform (flm:vec3 0 step 0) :replace-p t))
      (:z (rotate transform (flm:vec3 0 0 step) :replace-p t)))))

(defmethod on-action-finish (action (name (eql 'rotate/reverse)))
  (when (cycle-p action)
    (replace-action action 'rotate)))
