(in-package #:colony)

;; The internal collider system in the engine.
(defclass collider-system ()
  ((%physics-config :reader physics-config
                    :initarg :physics-config
                    :initform nil
                    :type (or-null physics-config))
   ;; Keyed by :layer-name,
   ;; Value is ht
   ;; Key of second ht is collider-ref
   ;; Value of second ht is collider-ref
   (%registering-colliders :reader registering-colliders
                           :initarg :registering-colliders
                           ;; keyed by on-layer in collider, value is a hash.
                           ;; second has is keyed by ref to collider and value
                           ;; is collider.
                           :initform (u:dict #'eq))
   ;; Stable colliders are ones that have already been registered.
   (%stable-colliders :reader stable-colliders
                      :initarg :stable-colliders
                      ;; keyed by on-layer in collider, value is a hash. second
                      ;; hash is keyed by ref to collider and value is
                      ;; collider.
                      :initform (u:dict #'eq))
   (%deregistering-colliders :reader deregistering-colliders
                             :initarg :deregistering-colliders
                             ;; keyed by on-layer in collider, value is a hash.
                             ;; second has is keyed by ref to collider and
                             ;; value is collider.
                             :initform (u:dict #'eq))
   ;; This is a buffer that we'll use to make colliding a layer against itself
   ;; faster and easier to compute.
   (%buffer :accessor buffer
            :initarg :buffer
            :initform (make-array 8 :adjustable t :fill-pointer t))
   ;; Are two colliders in collision?
   ;; When they enter this table, ON-COLLISION-ENTER is called.
   ;; When they stay in this table, ON-COLLISION-CONTINUE is called.
   ;; When they leave this table, ON-COLLISION-EXIT is called.
   ;; key is an collider, value is a hash of colliders with which the key is
   ;; colliding. When new colliders enter or leave, a few things must be
   ;; updated to be consistent.
   (%contacts :reader contacts
              :initarg :contacts
              :initform (u:dict #'eq))))
