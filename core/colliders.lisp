(in-package :%first-light)

;; NOTE: This stuff prolly should go into core/colliders.lisp
(defclass collider-system ()
  ((%layers :reader layers
            :initarg :layers
            :initform nil)
   (%collision-plan :reader collision-plan
                    :initarg :plan
                    ;; keyed by layer, value is list of layers it collides with
                    :initform (au:dict #'eq))
   (%colliders :reader colliders
               :initarg :colliders
               ;; keyed by on-layer in collider, value is a hash.
               ;; second has is keyed by ref to collider and value is collider.
               :initform (au:dict #'eq))
   ;; Are two colliders in collision?
   ;; When they enter this table, ON-COLLISION-ENTER is called.
   ;; When they stay in this table, ON-COLLISION-CONTINUE is called.
   ;; When they leave this table, ON-COLLISION-EXIT is called.
   ;; key is an collider, value is a hash of colliders with which the key is
   ;; colliding. When new colliders enter or leave, a few things must be
   ;; updated to be consistent.
   (%contacts :reader contacts
              :initarg :contacts
              :initform (au:dict #'eq))))

(defun make-collider-system (&rest init-args)
  (apply #'make-instance 'collider-system init-args))

(defmethod register-collider ((cs collider-system) collider)
  "Add a new collider that may participate in the collision system."
  ;; If the hash table for the layer doesn't exist, make it.
  (unless (au:href (colliders cs) (fl.comp:on-layer collider))
    (setf (au:href (colliders cs) (fl.comp:on-layer collider)) (au:dict #'eq)))

  ;; insert the collider into the collider-system's table
  (setf (au:href (colliders cs) (fl.comp:on-layer collider) collider)
        collider))

;; TODO: Add a new component Cfs-XXX state for computing physics collisions that
;; happens just after disabling a component. NOTE: Think about this more.
(defmethod unregister-collider ((cs collider-system) collider)
  "Remove a collider from participating in the collision system."
  ;; If the collier exists, remove it, but not the hash table for the layer.
  (when (au:href (colliders cs) (fl.comp:on-layer collider))
    (remhash collider (au:href (colliders cs) (fl.comp:on-layer collider)))))


(defmethod add-contact ((cs collider-system) collider-alpha collider-beta)
  (declare (ignore cs collider-alpha collider-beta))
  nil)

(defmethod remove-contact ((cs collider-system) collider)
  (declare (ignore cs collider))
  nil)


;;
;; TODO: This needs to be in a DSL like define-physics or something.
;;
;; The collision plan describes a lower left triangle of a
;; collision matrix with the indicies in both axes being in
;; the same order as the :layers list. @ means that isn't a valid
;; spot to put either a space (for these two do not collide) or an X
;; (which means these two do collide).
;;
;; Here is a collision system: (notice :scenery collides with nothing)
;;
;;               :ground :player :player-bullet :enemy :enemy-bullet :scenery
;;                -----------------------------------------------------------
;; :ground        | X       @           @          @         @           @
;; :player        | X                   @          @         @           @
;; :player-bullet | X                              @         @           @
;; :enemy         | X       X           X                    @           @
;; :enemy-bullet  | X       X           X                                @
;; :scenery       |
;;
;; Then, the collision plan is an sparse representation of which columns for
;; each row are marked X.

(defun init-collider-system (core-state)
  (let ((collider-system-desc
          '(:layers
            (:ground :player :player-bullet :enemy :enemy-bullet :scenery)

            ;; NOTE: Described above.
            :collision-plan
            ;; The FIRST is the row header, the SECOND is the X locations
            ;; that indicate a collision situation.
            (:ground (:ground)
             :player (:ground)
             :player-bullet (:ground)
             :enemy (:ground :player :player-bullet)
             :enemy-bullet (:ground :player :player-bullet)
             :scenery ()))))

    (setf (collider-system core-state)
          (make-collider-system
           :layers (getf collider-system-desc :layers)
           :collision-plan (getf collider-system-desc :collision-plan)))))
