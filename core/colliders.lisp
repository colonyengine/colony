(in-package :%first-light)

;; TODO: This is a naive collider resolution system that doesn't even take into
;; consideration or provide the feature of a sleeping collider. Also the main
;; algorithm in N^2 worst case. We take great care in handling the entrance and
;; exits of colliders as they enter this system to prevent multiple events from
;; being sent to referents of colliders and to correctly handle the removal (via
;; whatever means, like destruction or detachment) of colliders from thier
;; actors.

(defclass collider-system ()
  ((%physics-layers :reader physics-layers
                    :initarg :physics-layers
                    :initform nil)
   (%collision-plan :reader collision-plan
                    :initarg :collision-plan
                    ;; keyed by layer, value is list of layers it collides with
                    :initform (au:dict #'eq))

   ;; Keyed by :layer-name,
   ;; Value is ht
   ;; Key of second ht is collider-ref
   ;; Value of second ht is collider-ref
   (%registering-colliders :reader registering-colliders
                           :initarg :registering-colliders
                           ;; keyed by on-layer in collider, value is a hash.
                           ;; second has is keyed by ref to collider and value
                           ;; is collider.
                           :initform (au:dict #'eq))
   ;; Stable colliders are ones that have already been registered.
   (%stable-colliders :reader stable-colliders
                      :initarg :stable-colliders
                      ;; keyed by on-layer in collider, value is a hash.  second
                      ;; has is keyed by ref to collider and value is collider.
                      :initform (au:dict #'eq))
   (%deregistering-colliders :reader deregistering-colliders
                             :initarg :deregistering-colliders
                             ;; keyed by on-layer in collider, value is a hash.
                             ;; second has is keyed by ref to collider and value
                             ;; is collider.
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


;; TODO: Add a new component Cfs-XXX state for computing physics collisions that
;; happens just after disabling a component. NOTE: Think about this more.


(defun register-collider (context collider)
  "Add a new collider that may participate in the collision system."
  (let* ((cs (collider-system (core context)))
         (registering-colliders (registering-colliders cs)))

    ;; Insert the request for processing.
    (setf (au:href registering-colliders (fl.comp:on-layer collider) collider)
          collider)))

(defun deregister-collider (context collider)
  "Mark that a collider is ready to leve the collision system."
  (let* ((cs (collider-system (core context)))
         (deregistering-colliders (deregistering-colliders cs)))

    ;; Insert the request for processing.
    (setf (au:href deregistering-colliders (fl.comp:on-layer collider) collider)
          collider)))


;; ;;;;;;;;;;;;;;;;;;;
;; contacts are symmetric in the internal data structures.
;; ;;;;;;;;;;;;;;;;;;;

(defun contact-p (collider-system fist-collider face-collider)
  "Return a generalized boolean if the FIST-COLLIDER and the FACE-COLLIDER
are currently in contact."
  (assert (not (eq fist-collider face-collider)))

  ;; since there is a symmetric link, I can check any one and be satisfied.
  (let ((contacts (contacts collider-system)))
    (when (au:href contacts fist-collider)
      (au:when-let (found-p (au:href contacts fist-collider face-collider))
        ;; generalized-boolean.
        found-p))))

(defun enter-contact (collider-system fist-collider face-collider)
  (assert (not (eq fist-collider face-collider)))

  (let ((contacts (contacts collider-system)))

    ;; First, we add a link: fist -> face.
    (unless (au:href contacts fist-collider)
      (setf (au:href contacts fist-collider) (au:dict)))
    (setf (au:href contacts fist-collider face-collider) face-collider)

    ;; Then we add a symmetric back link: face -> fist.
    (unless (au:href contacts face-collider)
      (setf (au:href contacts face-collider) (au:dict)))
    (setf (au:href contacts face-collider fist-collider) fist-collider)

    ;; Now that the contact has been added we'll invoke the enter
    ;; protocol for the contact.
    (au:when-let (referent (fl.comp:referent fist-collider))
      (on-collision-enter referent face-collider))

    (au:when-let (referent (fl.comp:referent face-collider))
      (on-collision-enter referent fist-collider))

    :enter))


(defun continue-contact (collider-system fist-collider face-collider)
  "Assuming the contact already exists, call the continue protocol for
the FIST-COLLIDER and FACE-COLLIDER."
  (declare (ignore collider-system))
  (assert (not (eq fist-collider face-collider)))

  ;; If we're continuing to collide, run the protocol!
  (au:when-let (referent (fl.comp:referent fist-collider))
    (on-collision-continue referent face-collider))

  (au:when-let (referent (fl.comp:referent face-collider))
    (on-collision-continue referent fist-collider))

  :ontinue)

(defun exit-contact (collider-system fist-collider face-collider)
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
        (remhash face-collider contacts)))

    ;; Now that the contact has been removed, invoke the exit protocol
    ;; for the exiting contacts.
    (au:when-let (referent (fl.comp:referent fist-collider))
      (on-collision-exit referent face-collider))

    (au:when-let (referent (fl.comp:referent face-collider))
      (on-collision-exit referent fist-collider))

    :exit))

(defun remove-all-contacts (collider-system fist-collider)
  "Remove the FIST-COLLIDER from the contacts db and any contacts it might
have had--and update all other faces too."
  (let ((contacts (contacts collider-system)))
    ;; Look up the fist to see if anything at all is contacting it.
    (au:when-let (face-set (au:href contacts fist-collider))
      ;; NOTE: get a list of the faces, since we'll be altering the hash tables
      ;; while iterating over the faces
      (let ((face-colliders (au:hash-keys face-set)))
        ;; Now force exit each symmetric contact, if present. This will handle
        ;; removing hash tables we don't need anymore.
        (dolist (face-collider face-colliders)
          (when (contact-p collider-system fist-collider face-collider)
            (exit-contact collider-system fist-collider face-collider)))))))

(defun compute-contact-state (collider-system
                              fist-collider face-collider)
  ;; 1. Compute if a collision happend.
  ;; 2. look into the contacts table and see what to do.
  (let ((collided-p (fl.comp:collide-p fist-collider face-collider))
        (contact-p (contact-p collider-system fist-collider face-collider)))

    ;; split up into clauses this way for easier understanding. The clauses
    ;; happen in order of what I speculate will be the most common to least
    ;; common (but it likely depends on the game a lot I'm sure.)

    (cond
      ((and (not collided-p) (not contact-p))
       ;; nothing to do cause nothing continues to happen with these two
       ;; colliders!
       nil)

      ((and collided-p contact-p)
       ;; The collision is continuing. Inform both colliders they are in
       ;; a continuing collision.
       (continue-contact collider-system fist-collider face-collider))

      ((and (not collided-p) contact-p)
       ;; We just stopped colliding with something.
       (exit-contact collider-system fist-collider face-collider))

      ((and collided-p (not contact-p))
       ;; We just started colliding with something.
       (enter-contact collider-system fist-collider face-collider)))))

;; a test funtion for now, normally the internal functions will be called at
;; different times in the flow.
(defun compute-all-collisions  (collider-system)
  ;; NOTE: This order is not arbitrary! I need to thikn hard about how
  ;; registering and deregistering colliders work in the same frame.  I may have
  ;; to deregister first, then stable, then register.  It depends on how I treat
  ;; a registering collider showing up in a frame with a deregistering
  ;; collider... do I register first and then deregister? Or vice versa?

  ;; We compute these first....
  (compute-stable-collisions collider-system)
  ;; and then add in the imcoming "diffs" next.
  (compute-registering-collisions collider-system)
  ;; We do deregistering last to catch the case where a collider existed for one
  ;; frame. We'll get an enter/exit protocol on it properly.
  (compute-deregistering-collisions collider-system))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stable collisions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-stable-collisions (collider-system)
  ;; Update the collision status for stable colliders in the order of the
  ;; layers. N^2 for now cause it is fast to write. There is no spatial
  ;; partitioning.
  (dolist (fist-layer (physics-layers collider-system))
    (let ((face-layers (au:href (collision-plan collider-system) fist-layer)))
      (loop :for face-layer :in face-layers
            :do (compute-stable-layer-collisions collider-system
                                                 fist-layer face-layer)))))

(defun compute-stable-layer-collisions (collider-system fist-layer face-layer)
  ;; NOTE: we have to be very careful about sending duplicate events for the
  ;; same contact. If A and B collide, A gets a message about B
  ;; colliding with it, and B gets a message about A colliding with it, and
  ;; we better not later collide B with A and reinvoke the protocol! This
  ;; is why we do the two different methods since if we did
  ;; do-hash-keys-pairwise with the same hash table for both entries, we'll
  ;; get duplicate contacts. Hence, we use map-combinations in that case.

  (if (eq fist-layer face-layer)
      ;; In this scenario, we map-combinations over the hash table containing
      ;; the stable-colliders that are going to collide with themselves. We do
      ;; it a little strangely to avoid producing even more garbage caused by
      ;; asking for the hash-keys or hash-values as a list--instead we use a
      ;; dynamically growing reusable buffer.

      ;; However this does two passes over the keys, so there's that....
      (let ((fists-and-faces
              (au:href (stable-colliders collider-system) fist-layer)))
        (when fists-and-faces
          (setf (fill-pointer (buffer collider-system)) 0)
          (au:do-hash-keys (fist/face fists-and-faces)
            (vector-push-extend fist/face (buffer collider-system)))

          ;; compute collisions between each _unique_ pair of fists-and-faces
          (when (>= (length (buffer collider-system)) 2)
            (au:map-combinations
             (lambda (vecpair)
               (compute-contact-state collider-system
                                      (aref vecpair 0)
                                      (aref vecpair 1)))
             (buffer collider-system)
             :length 2
             ;; NOTE: :copy nil will reuse the same vector for each invocation
             ;; of the function, so very little garbage produced.
             :copy nil))))

      ;; ELSE simply iterate pairwise each fist collider over all the face
      ;; colliders. No chance of duplicate invocation of protocol here.
      (let ((fists (au:href (stable-colliders collider-system) fist-layer))
            (faces (au:href (stable-colliders collider-system) face-layer)))
        (when (and fists
                   faces
                   (> (hash-table-count fists) 0)
                   (> (hash-table-count faces) 0))
          (do-hash-keys-pairwise
              (lambda (fist face)
                (compute-contact-state collider-system fist face))
            fists faces)))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registering collisions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-registering-collisions (collider-system)
  ;; For each registering collider, collide it against every appropriate
  ;; collision layer in the stable colliders--computing the contact state
  ;; as necessary. Then, move the registering collider into the stable
  ;; collider state. This should only allow non-duplicate contact discovery.
  ;; We can go out of order, but choose to process the registering colliders
  ;; in the order of the physics-layers.

  (let ((registering-colliders (registering-colliders collider-system))
        (stable-colliders (stable-colliders collider-system)))

    ;; First, we process each physics-layer in order.
    ;; TODO: This is O(n) each time, we could do a better job of knowing
    ;; what registering colliders we actually have when there are a LOT
    ;; of physics-layers.
    (dolist (fist-layer (physics-layers collider-system))
      ;; We will be processing each registering fist collider of the current
      ;; fist-layer.
      (let ((fist-layer-registering-colliders
              (au:href registering-colliders fist-layer)))
        (unless (zerop (hash-table-count fist-layer-registering-colliders))
          ;; Now for each fist in our specified fist-layer, we'll collide it
          ;; against each collider in every single face physics layer its
          ;; on-layer slot implies.
          (au:do-hash-keys (fist fist-layer-registering-colliders)
            ;; we're processing it right now, so no matter what happens we
            ;; remove it from the registering-colliders.
            (remhash fist fist-layer-registering-colliders)

            ;; If somehow it is also currently stable, we totally ignore it.
            (unless (au:href stable-colliders fist-layer fist)
              ;; The FIST is good to go! collide it and stabilize it!
              (let ((face-layers
                      (au:href (collision-plan collider-system) fist-layer)))
                #++(format t "Checking registering fist: ~S, [~S: ~S]~%"
                           (fl.comp:name fist) (fl.comp:on-layer fist)
                           face-layers)
                (cond
                  ((null face-layers)
                   ;; If no face layers to collide against AT ALL,
                   ;; automatically stabilize the fist and we're done with it.
                   (format t " Stabilizing[0]: ~S~%" (fl.comp:name fist))
                   (setf (au:href stable-colliders fist-layer fist)
                         fist))

                  (t
                   ;; Else, we collide the fist against each face in each
                   ;; layer.
                   (dolist (face-layer face-layers)
                     #++(format t " Checking contacts between layers: ~S <=> ~S~%"
                                fist-layer face-layer)
                     ;; Find all the face-layer colliders to which we need to
                     ;; collide.
                     (let ((face-layer-stable-colliders
                             (au:href stable-colliders face-layer)))

                       ;; Do the work of colliding the single fist to all
                       ;; faces in this face-layer.
                       (unless (zerop (hash-table-count
                                       face-layer-stable-colliders))
                         (au:do-hash-keys (face face-layer-stable-colliders)
                           #++(format t "  compute-contact-state: [reg: ~S <-> stable: ~S]~%"
                                      (fl.comp:name fist) (fl.comp:name face))
                           (compute-contact-state collider-system fist
                                                  face)))))
                   ;; And when we *FINISH* colliding the specific registering
                   ;; fist against all of the stable faces in all face-layers
                   ;; its on-layer implied, *THEN* we stabilize the fist. This
                   ;; is so the next registering fist can collide against it if
                   ;; need be. NOTE: We CANNOT stabilize until AFTER the
                   ;; registering fist has been collided with all stable faces.
                   #++(format t " Stabilizing[1]: ~S~%" (fl.comp:name fist))
                   (setf (au:href stable-colliders fist-layer fist)
                         fist)))))))))))



(defun compute-deregistering-collisions (collider-system)
  ;; For each deregistering collider, remove it from the stable-colliders and
  ;; remove it from registering-colliders.  Then remove all contacts for it. We
  ;; don't need to do this in any particular order but we choose the physics
  ;; layer ordering for convenience.
  (let ((registering-colliders (registering-colliders collider-system))
        (stable-colliders (stable-colliders collider-system))
        (deregistering-colliders (deregistering-colliders collider-system)))

    (dolist (fist-layer (physics-layers collider-system))
      (let ((fist-layer-deregistering-colliders
              (au:href deregistering-colliders fist-layer)))
        (unless (zerop (hash-table-count fist-layer-deregistering-colliders))
          (au:do-hash-keys (fist fist-layer-deregistering-colliders)
            ;; 1. Remove from stable-colliders, cause it is leaving.
            (remhash fist (au:href stable-colliders fist-layer))
            ;; 2. Remove from registering-colliders. Either they have been
            ;; processed by now and this is empty or they will never be
            ;; processed cause the collider is leaving.
            (remhash fist (au:href registering-colliders fist-layer))
            ;; 3. Break all current fist contacts (if any)
            (remove-all-contacts collider-system fist)
            ;; 4. Now remove from myself, cause I just processed it.
            (remhash fist fist-layer-deregistering-colliders)))))))

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

(defun initialize-collider-system (core)
  (let* ((collider-system-desc
           ;; TODO: This should be a DSL in a define-physics macro or something.
           `(:physics-layers
             (:ground :player :player-bullet :enemy :enemy-bullet :scenery)

             ;; NOTE: FOrmat and legality described in comment above.
             :collision-plan
             ;; The KEY is the row header, the VALUE is the X locations
             ;; that indicate a collision situation.
             ,(au:dict
               :ground (list :ground)
               :player (list :ground)
               :player-bullet (list :ground)
               :enemy (list :ground :player :player-bullet)
               :enemy-bullet (list :ground :player :player-bullet)
               :scenery (list))))
         (new-collider-system
           (apply #'make-collider-system collider-system-desc)))

    (setf (collider-system core) new-collider-system)

    (with-accessors ((registering-colliders registering-colliders)
                     (stable-colliders stable-colliders)
                     (deregistering-colliders deregistering-colliders))
        new-collider-system
      (dolist (physics-layer (physics-layers new-collider-system))
        ;; Set up the htables to receive the collider references for each
        ;; physics-layer
        (setf (au:href registering-colliders physics-layer) (au:dict #'eq)
              (au:href stable-colliders physics-layer) (au:dict #'eq)
              (au:href deregistering-colliders physics-layer) (au:dict #'eq))))))

;; TODO: Move to utility file, maybe put in goldern-utils. Possibly extend to
;; take &rest and perform all combinations of keys/values/etc in hash table.
;; transfer-func deals with moving a from ht0 to ht1.
(defun do-hash-keys-pairwise (func ht0 ht1)
  (au:do-hash-keys (a ht0)
    (au:do-hash-keys (b ht1)
      (funcall func a b))))


;; TODO: Keep going.

(defun test-collider-system ()
  (let* ((core (make-instance 'core))
         (context (make-instance 'context :core core)))

    (with-slots (%context) core
      (setf %context context))

    (let* ((c0 (make-component (context core) 'fl.comp:collider/sphere
                               :name "Ground"
                               :on-layer :ground
                               :center (m:vec3 0 0 0)
                               :radius 1))
           (c1 (make-component (context core) 'fl.comp:collider/sphere
                               :name "Player"
                               :on-layer :player
                               :center (m:vec3 -20 5 0)
                               :radius 1))
           (c2 (make-component (context core) 'fl.comp:collider/sphere
                               :name "Player-Bullet"
                               :on-layer :player-bullet
                               :center (m:vec3 -10 5 0)
                               :radius 1))
           (c3 (make-component (context core) 'fl.comp:collider/sphere
                               :name "Enemy"
                               :on-layer :enemy
                               :center (m:vec3 20 5 0)
                               :radius 1))
           (c4 (make-component (context core) 'fl.comp:collider/sphere
                               :name "Enemy-Bullet"
                               :on-layer :enemy-bullet
                               :center (m:vec3 10 5 0)
                               :radius 1))
           (c5 (make-component (context core) 'fl.comp:collider/sphere
                               :name "Scenery 1"
                               :on-layer :scenery
                               :center (m:vec3 0 5 0)
                               :radius 1))
           (c6 (make-component (context core) 'fl.comp:collider/sphere
                               :name "Scenery 2"
                               :on-layer :scenery
                               :center (m:vec3 1 5 0)
                               :radius 1)))


      ;; Set referent to the same component for
      (loop :for c :in (list c0 c1 c2 c3 c4 c5 c6)
            :do (setf (fl.comp:referent c) c))

      (initialize-collider-system core)

      (register-collider context c0)
      (register-collider context c1)
      (register-collider context c2)
      (register-collider context c3)
      (register-collider context c4)
      (register-collider context c5)
      (register-collider context c6)

      (format t "Collider Pass 0: no colliding~%")
      (compute-all-collisions (collider-system core))

      (format t "Collider Pass 1: enter~%")
      (format t "Moving enemy-bullet.~%")
      (setf (fl.comp:center c4) (m:vec3 -9 5 0))
      (compute-all-collisions (collider-system core))


      (format t "Collider Pass 2: continue~%")
      (format t "Moving enemy-bullet.~%")
      (setf (fl.comp:center c4) (m:vec3 -10 5 0))
      (compute-all-collisions (collider-system core))

      (format t "Collider Pass 2a: continue~%")
      (format t "Moving enemy-bullet.~%")
      (setf (fl.comp:center c4) (m:vec3 -11 5 0))
      (compute-all-collisions (collider-system core))


      (format t "Collider Pass 2b: continue~%")
      (format t "Moving enemy-bullet.~%")
      (setf (fl.comp:center c4) (m:vec3 -12 5 0))
      (compute-all-collisions (collider-system core))

      (format t "Moving enemy-bullet.~%")
      (setf (fl.comp:center c4) (m:vec3 -13 5 0))
      (format t "Collider Pass 3: exit~%")
      (compute-all-collisions (collider-system core))

      (format t "Collider Pass 4: no colliding~%")
      (compute-all-collisions (collider-system core))

      (deregister-collider context c0)
      (deregister-collider context c1)
      (deregister-collider context c2)
      (deregister-collider context c3)
      (deregister-collider context c4)
      (deregister-collider context c5)
      (deregister-collider context c6)

      (collider-system core))))
