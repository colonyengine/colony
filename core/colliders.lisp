(in-package :%first-light)

;; TODO: This is a naive collider resolution system that doesn't even take into
;; consideration or provide the feature of a sleeping collider. Also the main
;; algorithm in N^2 worst case.

(defclass collider-system ()
  ((%layers :reader layers
            :initarg :layers
            :initform nil)
   (%collision-plan :reader collision-plan
                    :initarg :collision-plan
                    ;; keyed by layer, value is list of layers it collides with
                    :initform (au:dict #'eq))
   ;; A hash table holding all possible colliders participating in the collider
   ;; system.
   (%colliders :reader colliders
               :initarg :colliders
               ;; keyed by on-layer in collider, value is a hash.
               ;; second has is keyed by ref to collider and value is collider.
               :initform (au:dict #'eq))
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


;; contacts are symmetric in the internal data structures.
(defun add-contact (collider-system fist-collider face-collider)
  (assert (not (eq fist-collider face-collider)))

  (let ((contacts (contacts collider-system)))

    ;; First, we add a link: fist -> face.
    (unless (au:href contacts fist-collider)
      (setf (au:href contacts fist-collider) (au:dict)))
    (setf (au:href contacts fist-collider face-collider) face-collider)

    ;; Then we add a symmetric back link: face -> fist.
    (unless (au:href contacts face-collider)
      (setf (au:href contacts face-collider) (au:dict)))
    (setf (au:href contacts face-collider fist-collider) fist-collider)))


(defun contact-p (collider-system fist-collider face-collider)
  ;; since there is a symmetric link, I can check any one and be satisfied.
  (assert (not (eq fist-collider face-collider)))

  (let ((contacts (contacts collider-system)))
    (when (au:href contacts fist-collider)
      (au:when-let (found-p (au:href contacts fist-collider face-collider))
        ;; generalized-boolean.
        found-p))))

(defun remove-contact (collider-system fist-collider face-collider)
  "Remove the contact between the FIST and the FACE."
  (assert (not (eq fist-collider face-collider)))

  (let ((contacts (contacts collider-system)))
    ;; Remove the link: fist -> face
    (au:when-let (face-set (au:href contacts fist-collider))
      (remhash face-collider face-set)
      ;; If the fist is colliding with nothing now, remove its table.
      (when (zerop (hash-table-count face-set))
        (remhash fist-collider contacts)))

    ;; Remove the link: face -> fist
    (au:when-let (fist-set (au:href contacts face-collider))
      (remhash fist-collider fist-set)
      ;; If the face is colliding with nothing now, remove its table.
      (when (zerop (hash-table-count fist-set))
        (remhash face-collider contacts)))))

(defun remove-all-contacts (collider-system fist-collider)
  "Remove the FIST-COLLIDER from the contacts db and any contacts it might
have had--and update all other faces too."
  (let ((contacts (contacts collider-system)))
    ;; Look up the fist to see if anything at all is contacting it.
    (au:when-let (face-set (au:href contacts fist-collider))
      ;; NOTE: get a list of the faces, since we'll be altering the hash tables
      ;; while iterating over the faces
      (let ((face-colliders (au:hash-keys face-set)))
        ;; Now remove each symmetric contact. This will handle removing
        ;; hash tables we don't need anymore.
        (dolist (face-collider face-colliders)
          (remove-contact collider-system fist-collider face-collider))))))





(defun compute-all-collisions (collider-system)
  ;; NOTE: we have to be very careful about sending duplicate events for the
  ;; same collision. If A and B collide, A gets a message about B
  ;; colliding with it, and B gets a message about A colliding with it, and
  ;; we better not collide B with A and resend the messages later!

  ;; Update the collision status for appropriate colliders in the order of the
  ;; layers. N^2 for now cause it is fast to write. There is no spatial
  ;; partitioning.
  (dolist (fist-layer (layers collider-system))
    (let ((face-layers (au:href (collision-plan collider-system) fist-layer)))
      (loop :for face-layer :in face-layers
            :do (compute-layer-collisions collider-system
                                          fist-layer face-layer)))))


(defun compute-layer-collisions (collider-system fist-layer face-layer)
  (cond
    ((eq fist-layer face-layer)
     ;; In this scenario, we map-combinations over the hash table containing
     ;; the colliders that are going to collide with themselves. We do it a
     ;; little strangely to avoid producing even more garbage by asking for the
     ;; hash-keys or hash-values as a list--we use a dynamically growing
     ;; buffer that we reuse after each attempt.

     ;; However this does two passes over the keys, so there's that.
     (let ((fists-and-faces (au:href (colliders collider-system) fist-layer)))
       (when fists-and-faces
         (setf (fill-pointer (buffer collider-system)) 0)
         (au:do-hash-keys (fist/face fists-and-faces)
           (vector-push-extend fist/face (buffer collider-system)))

         ;; compute collisions between each unique pair of fists-and-faces
         (when (>= (length (buffer collider-system)) 2)
           (au:map-combinations
            (lambda (vecpair)
              (compute-collision-state collider-system
                                       (aref vecpair 0)
                                       (aref vecpair 1)))
            (buffer collider-system)
            :length 2
            :copy nil)))))
    (t
     ;; otherwise simply iterate each fist collider over all the face
     ;; colliders.
     (let ((fists (au:href (colliders collider-system) fist-layer))
           (faces (au:href (colliders collider-system) face-layer)))
       (when (and fists
                  faces
                  (> (hash-table-count fists) 0)
                  (> (hash-table-count faces) 0))
         (au:do-hash-keys (fist fists)
           (au:do-hash-keys (face faces)
             (compute-collision-state collider-system fist face))))))))


(defun compute-collision-state (collider-system
                                fist-collider face-collider)
  ;; 1. Compute if a collision happend.
  ;; 2. look into the contacts table and see what to do.
  (let ((collided-p (fl.comp:collide-p fist-collider face-collider))
        (contact-p (contact-p collider-system fist-collider face-collider)))

    ;; split up into clauses this way for easier understanding. The clauses
    ;; happen in order of what I speculate will be the most common to least
    ;; common.

    (cond
      ((and (not collided-p) (not contact-p))
       ;; nothing to do cause nothing continues to happen with these two
       ;; colliders!
       nil)

      ((and collided-p contact-p)
       ;; The collision is continuing. Inform both colliders they are in
       ;; a continuing collision.
       (au:when-let (referent (fl.comp:referent fist-collider))
         (on-collision-continue referent face-collider))
       (au:when-let (referent (fl.comp:referent face-collider))
         (on-collision-continue referent fist-collider)))

      ((and (not collided-p) contact-p)
       ;; We just stopped colliding with something.
       (remove-contact collider-system fist-collider face-collider)
       (au:when-let (referent (fl.comp:referent fist-collider))
         (on-collision-exit referent face-collider))
       (au:when-let (referent (fl.comp:referent face-collider))
         (on-collision-exit referent fist-collider)))

      ((and collided-p (not contact-p))
       ;; We just started colliding with something.
       (add-contact collider-system fist-collider face-collider)
       (au:when-let (referent (fl.comp:referent fist-collider))
         (on-collision-enter referent face-collider))
       (au:when-let (referent (fl.comp:referent face-collider))
         (on-collision-enter referent fist-collider))))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;
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
          `(:layers
            (:ground :player :player-bullet :enemy :enemy-bullet :scenery)

            ;; NOTE: Described above.
            :collision-plan
            ;; The KEY is the row header, the VALUE is the X locations
            ;; that indicate a collision situation.
            ,(au:dict
              :ground (list :ground)
              :player (list :ground)
              :player-bullet (list :ground)
              :enemy (list :ground :player :player-bullet)
              :enemy-bullet (list :ground :player :player-bullet)
              :scenery (list)))))

    (setf (collider-system core-state)
          (make-collider-system
           :layers (getf collider-system-desc :layers)
           :collision-plan (getf collider-system-desc :collision-plan)))))

(defun test-collider-system ()
  (let ((core-state (make-instance 'core-state)))

    (with-slots (%context) core-state
      (setf %context (make-instance 'context :core-state core-state)))

    (let ((c0 (make-component (context core-state) 'fl.comp:collider/sphere
                              :on-layer :ground
                              :referent :a
                              :center (m:vec3 0 0 0)
                              :radius 1))
          (c1 (make-component (context core-state) 'fl.comp:collider/sphere
                              :on-layer :player
                              :referent :a
                              :center (m:vec3 -5 3 0)
                              :radius 1))
          (c2 (make-component (context core-state) 'fl.comp:collider/sphere
                              :on-layer :player-bullet
                              :referent :a
                              :center (m:vec3 -2 3 0)
                              :radius 1))
          (c3 (make-component (context core-state) 'fl.comp:collider/sphere
                              :on-layer :enemy
                              :referent :a
                              :center (m:vec3 5 3 0)
                              :radius 1))
          (c4 (make-component (context core-state) 'fl.comp:collider/sphere
                              :on-layer :enemy-bullet
                              :referent :a
                              :center (m:vec3 -1 3 0)
                              :radius 1))
          (c5 (make-component (context core-state) 'fl.comp:collider/sphere
                              :on-layer :enemy-bullet
                              :referent :a
                              :center (m:vec3 3 3 0)
                              :radius 1))
          (c6 (make-component (context core-state) 'fl.comp:collider/sphere
                              :on-layer :scenery
                              :referent :a
                              :center (m:vec3 -1 0 0)
                              :radius 1)))
      (init-collider-system core-state)

      (register-collider (collider-system core-state) c0)
      (register-collider (collider-system core-state) c1)
      (register-collider (collider-system core-state) c2)
      (register-collider (collider-system core-state) c3)
      (register-collider (collider-system core-state) c4)
      (register-collider (collider-system core-state) c5)
      (register-collider (collider-system core-state) c6)

      (format t "Collider Pass 1~%")

      (compute-all-collisions (collider-system core-state))

      (format t "Collider Pass 2~%")

      (compute-all-collisions (collider-system core-state))

      (format t "Moving enemy-bullet.~%")
      (setf (fl.comp:center c4) (m:vec3 -1 5 0))
      (compute-all-collisions (collider-system core-state))

      (collider-system core-state))))
