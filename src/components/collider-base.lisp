(in-package #:colony.component)

;; This should probably should be an actual define-component, but
;; inheritance and how the engine handles that kind of thing in the type
;; graph is... not well defined yet.

;; This type is one of the parents for all collider components.
;; TODO: We do not have a difference between triggers and collisions yet.
;; That will come when actual physics arrives.
;; on-collision-enter
;; on-collision-continue
;; on-collision-exit
;; We'll need to add soon:
;; on-trigger-enter
;; on-trigger-continue
;; on-trigger-exit
;;
;; TODO: We cannot put the 'material' slot from the cuboid or sphere
;; collider components here because they use the :annotation system and
;; that is for components only, not regular defclasses. So currently
;; that slot is duplicated in those colliders as opposed to having been
;; refactored to here.
(defclass collider ()
  ;; The collider is only ever on a single layer.
  ((%on-layer :accessor on-layer
              :initarg :on-layer)
   ;; How many things are contacting the collider.
   (%contact-count :accessor contact-count
                   :initform 0)
   ;; The component ref, if available, that will be used in the collision
   ;; protocol.
   (%referent :accessor referent
              :initarg :referent
              :initform nil)


   ;; ----------------------------------------------------------------------
   ;; TODO: Internal debugging stuff, needs to move to different place later.

   ;; TODO: This block of slots are really here for debugging drawing of a
   ;; collider hack on it a bit to make it better.
   (%visualize :accessor visualize
               :initarg :visualize
               :initform nil)
   ;; TODO: Put geometry into storage for all colliders's to use.
   (%geometry :reader geometry
              :initform (gl:gen-vertex-array))

   ;; TODO: Debugging material slot should be here, but the annotation system
   ;; doesn't yet work with defclass.
   ))
