(in-package :first-light.components)

(define-component collider/sphere ()
  (;; The collider is only ever on a single layer.
   (on-layer :default nil)
   ;; XXX there is no difference between trigger colliders and non trigger
   ;; colliders because we don't have physics yet.

   (center :default (m:vec3))
   (radius :default 1.0)
   ;; The component that handles the collision events.
   ;; That component better have the right methods defined for it.
   ;; on-collision-enter
   ;; on-collision-exit
   (collision-event-handler :default nil)))


(defmethod on-component-initialize ((self collider/sphere))
  ;; register to communal collider db
  (let ((context (context self)))
    (declare (ignore context))

    nil))

(defmethod collide-p ((self collider/sphere) (other collider/sphere))
  "Return T if the two collider/spheres actually collided."
  (let ((context (context self)))
    (declare (ignore context))


    nil))

(defmethod on-component-destroy ((self collider/sphere))
  ;; Unregister from communal collider db
  (let ((context (context self)))
    (declare (ignore context))

    nil))
