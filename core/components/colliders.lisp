(in-package :first-light.components)

(define-component collider/sphere ()
  (;; The collider is only ever on a single layer.
   (on-layer :default nil)
   (center :default (m:vec3))
   (radius :default 1.0)

   ;; TODO: We do not have a difference between triggers and collisions yet.
   ;; That will come when actual physics arrives.
   ;; on-collision-enter
   ;; on-collision-continue
   ;; on-collision-exit
   (referent :default nil)))

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
