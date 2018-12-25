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

(fl:define-component sprite-sheet ()
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
  (fl:with-shared-storage
      (context (fl:context sprite-sheet))
      ((ssbo/spec-data ssbo-presentp
                       ;; Which type storage to look at
                       ('sprite-sheet
                        ;; which shared-storage namespace
                        :ssbo/specification-data
                        ;; the key(s) to look up.
                        (spec-resource-id sprite-sheet))

                       ;; If not present insert result of this
                       ;; form at that above key into shared storage.
                       (fl.shader:create-buffer (fl.util:unique-name :sprite-sheet-buffer)
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
            :finally (fl.shader:write-buffer-path
                      (fl.shader:buffer-name ssbo/spec-data) :x xs)
                     (fl.shader:write-buffer-path
                      (fl.shader:buffer-name ssbo/spec-data) :y ys)
                     (fl.shader:write-buffer-path
                      (fl.shader:buffer-name ssbo/spec-data) :w ws)
                     (fl.shader:write-buffer-path
                      (fl.shader:buffer-name ssbo/spec-data) :h hs)))

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
    (fl.shader:bind-block 'ssbo/specification-data 9)))

(defun convert-current-sprite (sprite-sheet)
  "Convert the current-animation and current-cell into an integer and store it in the sprite sheet
for later use with the shaders."
  (with-slots (%animvec %current-animation %current-cell) (animations sprite-sheet)
    (setf (sprite sprite-sheet)
          (fl.util:href (sprites sprite-sheet)
                        (aref (aref %animvec %current-animation) (1+ %current-cell))))))

(defmethod fl:on-component-initialize ((self sprite-sheet))
  (with-accessors ((context fl:context) (actor fl:actor) (spec-resource-id spec-resource-id)
                   (spec spec) (material material) (transform transform) (vao-id vao-id))
      self
    (let ((path (fl:find-resource context spec-resource-id)))
      (setf spec (fl.util:safe-read-file-form path)
            transform (fl:actor-component-by-type actor 'fl.comp:transform)
            vao-id (gl:gen-vertex-array))
      (make-sprite-sheet-buffer self)
      (convert-current-sprite self))))

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

(defmethod fl:on-component-update ((self sprite-sheet))
  (let ((context (fl:context self)))
    (maybe-update-current-animation-cell self (fl:frame-time context))))

(defmethod fl:on-component-render ((self sprite-sheet))
  (with-accessors ((context fl:context) (spec-resource-id spec-resource-id) (transform transform)
                   (material material) (sprite sprite) (vao-id vao-id))
      self
    (fl.util:when-let ((camera (fl:active-camera context)))
      ;; Bind the appropriate buffer associated with this specific sprite-sheet
      ;; to the shader block.
      (let ((ssbo/spec-data
              (fl:ss-href context 'sprite-sheet :ssbo/specification-data spec-resource-id)))
        (fl.shader:bind-buffer (fl.shader:buffer-name ssbo/spec-data) 9))
      (fl:using-material material
          (:model (fl.comp:model transform)
           :view (fl.comp:view camera)
           :proj (fl.comp:projection camera)
           :tex.sprite sprite)
        (gl:bind-vertex-array vao-id)
        (gl:draw-arrays :points 0 1)))))

(fl:define-component simple-movement ()
  ((transform :default nil)))

(defmethod fl:on-component-initialize ((self simple-movement))
  (with-accessors ((actor fl:actor) (transform transform)) self
    (setf transform (fl:actor-component-by-type actor 'transform))))

(defmethod fl:on-component-update ((self simple-movement))
  (with-accessors ((context fl:context) (transform transform)) self
    (fl.util:mvlet* ((lx ly (fl.input:get-gamepad-analog (fl:input-data context)
                                                         '(:gamepad1 :left-stick)))
                     (rx ry (fl.input:get-gamepad-analog (fl:input-data context)
                                                         '(:gamepad1 :right-stick))))
      (let ((vec (flm:vec3 lx ly 0)))
        (flm:* (if (> (flm:length vec) 1) (flm:normalize vec) vec) 150.0 vec)
        (fl.comp:translate transform (flm:+ (flm:vec3 -400 0 0) vec) :replace-p t))
      (unless (= rx ry 0.0)
        (let* ((angle (atan (- rx) ry))
               ;; keep angle from 0 to 2pi for easier debugging of other things.
               (angle (if (< angle 0)
                          (+ pi (- pi (abs angle)))
                          angle)))
          (fl.comp:rotate transform (flm:vec3 0 0 angle) :replace-p t))))))

(fl:define-component shot-mover ()
  ((transform :default nil)
   (velocity :default 0)))

(defmethod fl:on-component-initialize ((self shot-mover))
  (with-accessors ((actor fl:actor) (transform transform)) self
    (setf transform (fl:actor-component-by-type actor 'fl.comp:transform))))

(defmethod fl:on-component-update ((self shot-mover))
  (with-accessors ((context fl:context) (transform transform) (velocity velocity)) self
    (fl.comp:translate
     transform
     (let ((a (flm:normalize (flm:vec3 (flm:get-column (fl.comp:local transform) 1))))
           (move-delta (* velocity (float (fl:frame-time context)))))
       (flm:* a move-delta)))))

(fl:define-component shot-emitter ()
  ((emitter-transform :default nil)))

(defmethod fl:on-component-initialize ((self shot-emitter))
  (with-accessors ((actor fl:actor) (emitter-transform emitter-transform)) self
    (setf emitter-transform (fl:actor-component-by-type actor 'fl.comp:transform))))

(defmethod fl:on-component-update ((self shot-emitter))
  (with-accessors ((context fl:context) (emitter-transform emitter-transform)) self
    (when (or (fl.input:input-enter-p (fl:input-data context) '(:gamepad1 :a))
              (fl.input:input-enter-p (fl:input-data context) '(:mouse :left)))
      (let* ((parent-model (fl.comp:model emitter-transform))
             (parent-translation (flm:get-translation parent-model))
             (parent-rotation (flm:quat parent-model))
             (new-actor (fl:make-actor context :id (fl.util:unique-name 'shot)))
             (transform (fl:make-component context
                                           'fl.comp:transform
                                           ;; here I init the local location/rotation
                                           ;; to semantically match the emitter
                                           ;; actor.
                                           :translate parent-translation
                                           :rotate parent-rotation))
             (shot-mover (fl:make-component context 'shot-mover :velocity 1000))
             (sprite (fl:make-component context
                                        'sprite-sheet
                                        :spec-resource-id :spritesheet-data
                                        :material 'sprite
                                        :animations (make-sprite-sheet-animations
                                                     0 0 #(#(.25
                                                             "bullet01"
                                                             "bullet02"))))))
        (fl:attach-multiple-components new-actor transform shot-mover sprite)
        ;; The shot is free in the universe.
        (fl:spawn-actor new-actor)
        ;; This is the method for destroying actors and components. Add to public
        ;; API. Don't use :ttl in the make-actor call yet.
        (%fl::destroy new-actor :ttl 1)))))
