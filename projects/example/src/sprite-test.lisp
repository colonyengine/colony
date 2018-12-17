(in-package :first-light.example)

;;;; This code is all a rough sketch about how to do your shader method and
;;;; implement a sprite sheet. This component is very inefficient cause shared
;;;; storage needs some more work--each instance has a vao/vbo, etc,
;;;; etc. Animations are specified clunkily, but are servicable.

(defclass sprite-sheet-animations ()
  ((%animvec :reader animvec
             :initarg :animvec
             :initform nil)
   (%elapsed-time :accessor elapsed-time
                  :initarg :elapsed-time
                  :initform 0.0)
   (%cell-time :accessor cell-time
               :initarg :cell-time)
   (%current-cell :accessor current-cell
                  :initarg :current-cell)
   (%current-animation :accessor current-animation
                       :initarg :current-animation)))

(defun make-sprite-sheet-animations (current-animation current-cell animation-vector)
  (let ((cell-time (/ (aref (aref animation-vector current-animation) 0)
                      (1- (length (aref animation-vector current-animation))))))
    (make-instance 'sprite-sheet-animations
                   :animvec animation-vector
                   :current-animation current-animation
                   :cell-time cell-time
                   :current-cell current-cell)))

(define-component sprite-sheet ()
  (;; Where this will be drawn, gotten from the actor on which this component is placed.
   (transform :default nil)
   ;; The description where things are in the sprite sheet.
   (spec-resource-id :default nil)
   ;; The actual specification form from the above file.
   (spec :default nil)
   ;; material describing the specific sprite sheet I want.
   (material :default nil
             :annotation (fl.annotations:material))
   ;; The ssbo buffer reference (possibly shared between instances).
   (ssbo-buffer :default nil)
   ;; The empty vao we create and draw
   (vao-id :default nil)
   ;; The integral location of the sprite suitable for the shader to draw it.
   (sprite :default nil)
   ;; mapping from sprite names to integral locations "ship33" -> 373 (actual
   ;; number may be different)
   (sprites :default (fl.util:dict #'equalp))
   ;; DB of animations and the cells involved in them.
   (animations :default nil))

  ;; Shared storage definitions.
  (;; Key: spec-resource-id location tuple,
   ;; Value: buffer-name of created buffer to hold it
   (:ssbo/specification-data equal)))

(defun make-sprite-sheet-buffer (sprite-sheet)
  ;; 1. I know the block-alias I need is:
  ;; since I wrote it in the material definition.
  ;; The material itself performed the creation of the above alias.

  ;; 2. create a buffer which must be unique for all buffers of the same spec.
  (with-shared-storage
      (context (context sprite-sheet))
      ((ssbo/spec-data ssbo-presentp
                       ;; Which type storage to look at
                       ('sprite-sheet
                        ;; which shared-storage namespace
                        :ssbo/specification-data
                        ;; the key(s) to look up.
                        (spec-resource-id sprite-sheet))

                       ;; If not present insert result of this
                       ;; form at that above key into shared storage.
                       (fl.shaderlib:create-buffer (fl.util:unique-name :sprite-sheet-buffer)
                                                   'ssbo/specification-data)))

    ;; Store a reference to the (possibly shared) buffer.
    (setf (ssbo-buffer sprite-sheet) ssbo/spec-data)

    ;; When ssbo-presentp is NIL, it means the cache was ust populated by the
    ;; required buffer, so we need to initialize it.
    (unless ssbo-presentp
      ;; 3. only a single instance should fill the GPU buffer appropriately the
      ;; first time it is created.
      (loop :with length = (length (spec sprite-sheet))
            :with xs = (make-array length :element-type 'single-float)
            :with ys = (make-array length :element-type 'single-float)
            :with ws = (make-array length :element-type 'single-float)
            :with hs = (make-array length :element-type 'single-float)
            :for sprite :in (spec sprite-sheet)
            :for i :from 0
            :do (destructuring-bind (&key id x y w h) sprite
                  (when (and id x y w h)
                    (setf (aref xs i) x
                          (aref ys i) y
                          (aref ws i) w
                          (aref hs i) h)))
            :finally
               (fl.shaderlib:write-buffer-path
                (fl.shaderlib:buffer-name ssbo/spec-data) :x xs)
               (fl.shaderlib:write-buffer-path
                (fl.shaderlib:buffer-name ssbo/spec-data) :y ys)
               (fl.shaderlib:write-buffer-path
                (fl.shaderlib:buffer-name ssbo/spec-data) :w ws)
               (fl.shaderlib:write-buffer-path
                (fl.shaderlib:buffer-name ssbo/spec-data) :h hs)))

    ;; Initialize the ids, we do this for each sprite-sheet instance we make
    ;; I could put this into a shared storage thing, but meh.
    ;; TODO: I should jam this into the shared storage too, since it is copied
    ;; for all sprite-sheets that use the same spec-resource-id.
    (loop :for sprite :in (spec sprite-sheet)
          :for i :from 0
          :do (destructuring-bind (&key id x y w h) sprite
                (when (and id x y w h)
                  (setf (fl.util:href (sprites sprite-sheet) id) i))))

    ;; TODO: I hacked a 9 in here as the binding point. Needs FL support
    ;; to be auto allocated.
    (fl.shaderlib:bind-block 'ssbo/specification-data 9)))


(defun convert-current-sprite (sprite-sheet)
  "Convert the current-animation and current-cell into an integer and store it in the sprite sheet
for later use with the shaders."
  (with-slots (%animvec %current-animation %current-cell) (animations sprite-sheet)
    (setf (sprite sprite-sheet)
          (fl.util:href (sprites sprite-sheet)
                        (aref (aref %animvec %current-animation) (1+ %current-cell))))))

(defmethod initialize-component ((sprite-sheet sprite-sheet))
  (with-slots (%spec-resource-id %spec %material %transform %vao-id) sprite-sheet
    (let* ((context (context sprite-sheet))
           (path (find-resource context %spec-resource-id)))
      (setf %spec (fl.util:safe-read-file-form path)
            %transform (actor-component-by-type (actor sprite-sheet) 'transform)
            %vao-id (gl:gen-vertex-array))
      (make-sprite-sheet-buffer sprite-sheet)
      (convert-current-sprite sprite-sheet))))

(defun maybe-update-current-animation-cell (sprite-sheet frame-time)
  (with-slots (%elapsed-time %cell-time %current-animation %current-cell %animvec)
      (animations sprite-sheet)
    ;; NOTE: I didn't look too closely at my thinking for this algorithm, so the
    ;; time calculations could be a little off.

    ;; Add how much time went by in the frame.
    (incf %elapsed-time frame-time)
    ;; Then, see if we're past the cell-time
    (when (>= %elapsed-time %cell-time)
      ;; go to the next cell until we consume all of the elapsed time.
      (loop :until (< %elapsed-time %cell-time)
            :do (setf %current-cell
                      (mod (1+ %current-cell)
                           (1- (length (aref %animvec %current-animation)))))
                (decf %elapsed-time %cell-time)
                (when (zerop %elapsed-time)
                  (setf %elapsed-time 0)))
      ;; fixup the current cell to show.
      (convert-current-sprite sprite-sheet))))

(defmethod update-component ((sprite-sheet sprite-sheet))
  (let ((context (context sprite-sheet)))
    (maybe-update-current-animation-cell sprite-sheet (frame-time context))))

(defmethod render-component ((sprite-sheet sprite-sheet))
  (with-slots (%transform %material %sprite %vao-id) sprite-sheet
    (let ((context (context sprite-sheet)))
      (fl.util:when-let ((camera (active-camera context)))
        ;; Bind the appropriate buffer associated with this specific sprite-sheet
        ;; to the shader block.
        (let ((ssbo/spec-data
                (ss-href context 'sprite-sheet :ssbo/specification-data
                         (spec-resource-id sprite-sheet))))
          (fl.shaderlib:bind-buffer (fl.shaderlib:buffer-name ssbo/spec-data) 9))

        (using-material %material
            (:model (fl.comp:model %transform)
             :view (fl.comp:view camera)
             :proj (fl.comp:projection camera)
             :tex.sprite %sprite)
          (gl:bind-vertex-array %vao-id)
          (gl:draw-arrays :points 0 1))))))

(define-component simple-movement ()
  ((transform :default nil)))

(defmethod initialize-component ((component simple-movement))
  (with-accessors ((actor actor) (transform transform)) component
    (setf transform (actor-component-by-type actor 'transform))))

(defmethod update-component ((component simple-movement))
  (with-slots (%transform) component
    (fl.util:mvlet* ((context (context component))
                     (lx ly (fl.input:get-gamepad-analog (input-data context)
                                                         '(:gamepad1 :left-stick)))
                     (rx ry (fl.input:get-gamepad-analog (input-data context)
                                                         '(:gamepad1 :right-stick))))
      (let ((vec (flm:vec3 lx ly 0)))
        (flm:* (if (> (flm:length vec) 1) (flm:normalize vec) vec) 150.0 vec)
        (fl.comp:translate %transform (flm:+ (flm:vec3 -400 0 0) vec) :replace-p t))
      (unless (= rx ry 0.0)
        (let* ((angle (atan (- rx) ry))
               ;; keep angle from 0 to 2pi for easier debugging of other things.
               (angle (if (< angle 0)
                          (+ pi (- pi (abs angle)))
                          angle)))
          (fl.comp:rotate %transform (flm:vec3 0 0 angle) :replace-p t))))))

(define-component shot-mover ()
  ((transform :default nil)
   (velocity :default 0)))

(defmethod initialize-component ((component shot-mover))
  (setf (transform component) (actor-component-by-type (actor component) 'transform)))

(defmethod update-component ((component shot-mover))
  (fl.comp:translate
   (transform component)
   ;; fly along the forward axis for this shot
   (let* ((context (context component))
          (a (flm:vec3 (flm:get-column (fl.comp:local (transform component)) 1)))
          (b (flm:normalize a))
          (move-delta (* (velocity component) (float (frame-time context)))))
     (flm:* b move-delta))))

(define-component shot-emitter ()
  ((emitter-transform :default nil)))

(defmethod initialize-component ((component shot-emitter))
  (setf (emitter-transform component)
        (actor-component-by-type (actor component) 'transform)))

(defmethod update-component ((component shot-emitter))
  (let ((context (context component)))
    (when (or (fl.input:input-enter-p (input-data context) '(:gamepad1 :a))
              (fl.input:input-enter-p (input-data context) '(:mouse :left)))
      (let* ((parent-model (fl.comp:model (emitter-transform component)))
             (parent-translation (flm:get-translation parent-model))
             (parent-rotation (from-scaled-mat4 parent-model))
             (new-actor (%fl::make-actor context :id (fl.util:unique-name 'shot)))
             (transform (make-component context
                                        'fl.comp:transform
                                        ;; here I init the local location/rotation
                                        ;; to semantically match the emitter
                                        ;; actor.
                                        :translation/current parent-translation
                                        :rotation/current parent-rotation))
             (shot-mover (make-component context 'shot-mover :velocity 1000))
             (sprite (make-component context
                                     'sprite-sheet
                                     :spec-resource-id :spritesheet-data
                                     :material 'sprite
                                     :animations (make-sprite-sheet-animations
                                                  0 0 #(#(.25
                                                          "bullet01"
                                                          "bullet02"))))))
        (attach-multiple-components new-actor transform shot-mover sprite)
        ;; The shot is free in the universe.
        (spawn-actor new-actor)
        ;; This is the method for destroying actors and components. Add to public
        ;; API. Don't use :ttl in the make-actor call yet.
        (%fl::destroy new-actor :ttl 1)))))

;; TODO: This should go into gamebox-math. It is an alternate form of
;; QUAT:FROM-MAT4 that can handle non-orthonormal rotation matricies.
;; Both should exist, and the user can use what they desire.

(defun from-scaled-mat4! (out matrix)
  "Convert scaled MATRIX to a quaternion, storing the result in the existing quaternion, OUT."
  (flm:with-quat ((q out))
    (flm:with-mat4 ((m matrix))
      ;; Normalize the basis vectors, but don't allocate vector memory,
      ;; and don't alter the original input matrix.
      (let* ((x-rot-denom (sqrt (+ (* m.00 m.00) (* m.10 m.10) (* m.20 m.20))))
             (y-rot-denom (sqrt (+ (* m.01 m.01) (* m.11 m.11) (* m.21 m.21))))
             (z-rot-denom (sqrt (+ (* m.02 m.02) (* m.12 m.12) (* m.22 m.22))))
             ;; normalize x-rotation basis vector
             (nm00 (/ m.00 x-rot-denom))
             (nm10 (/ m.10 x-rot-denom))
             (nm20 (/ m.20 x-rot-denom))
             ;; normalize y-rotation basis vector
             (nm01 (/ m.01 y-rot-denom))
             (nm11 (/ m.11 y-rot-denom))
             (nm21 (/ m.21 y-rot-denom))
             ;; normalize z-rotation basis vector
             (nm02 (/ m.02 z-rot-denom))
             (nm12 (/ m.12 z-rot-denom))
             (nm22 (/ m.22 z-rot-denom)))
        ;; Use the newly normalized values.
        (let ((trace (+ nm00 nm11 nm22 m.33))
              (col1 (1+ (- nm00 nm11 nm22)))
              (col2 (1+ (- nm11 nm00 nm22)))
              (col3 (1+ (- nm22 nm00 nm11)))
              (s 0.0f0))
          (cond
            ((plusp trace)
             (setf s (/ 0.5f0 (sqrt trace))
                   q.w (/ 0.25f0 s)
                   q.x (* (- nm21 nm12) s)
                   q.y (* (- nm02 nm20) s)
                   q.z (* (- nm10 nm01) s)))
            ((and (>= col1 col2) (>= col1 col3))
             (setf s (/ 0.5f0 (sqrt col1))
                   q.w (* (- nm21 nm12) s)
                   q.x (/ 0.25f0 s)
                   q.y (* (+ nm10 nm01) s)
                   q.z (* (+ nm02 nm20) s)))
            ((and (>= col2 col1) (>= col2 col3))
             (setf s (/ 0.5f0 (sqrt col2))
                   q.w (* (- nm02 nm20) s)
                   q.x (* (+ nm01 nm10) s)
                   q.y (/ 0.25f0 s)
                   q.z (* (+ nm12 nm21) s)))
            (t
             (setf s (/ 0.5f0 (sqrt col3))
                   q.w (* (- nm10 nm01) s)
                   q.x (* (+ nm02 nm20) s)
                   q.y (* (+ nm12 nm21) s)
                   q.z (/ 0.25f0 s)))))))
    out))

(defun from-scaled-mat4 (matrix)
  "Convert scaled MATRIX to a quaternion, storing the result in a freshly allocated quaternion."
  (from-scaled-mat4! flm:+id-quat+ matrix))
