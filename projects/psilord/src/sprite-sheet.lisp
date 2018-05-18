(in-package :fl.psilord)

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
   (spec-path :default nil)
   ;; The actual specification form from the above file.
   (spec :default nil)
   ;; material describing the specific sprite sheet I want.
   (material :default nil)
   ;; The empty vao we create and draw
   (vao-id :default nil)
   ;; The empty vertex buffer object we draw to initiate shader generation.
   (vbo-id :default nil)
   ;; The integral location of the sprite suitable for the shader to draw it.
   (sprite :default nil)
   ;; mapping from sprite names to integral locations  "ship33" -> 373 (actual number may be
   ;; different)
   (sprites :default (au:dict #'equalp))
   ;; DB of animations and the cells involved in them.
   (animations :default nil)))

(defun make-sprite-sheet-buffer (sprite-sheet)

  (shadow:create-buffer :ssbo :sprite-sheet
                        'fl.psilord.shaders:sprite-shader :sprite-sheet)

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
                      (aref hs i) h
                      (au:href (sprites sprite-sheet) id) i)))
        :finally (shadow:write-buffer-path :sprite-sheet :x xs)
                 (shadow:write-buffer-path :sprite-sheet :y ys)
                 (shadow:write-buffer-path :sprite-sheet :w ws)
                 (shadow:write-buffer-path :sprite-sheet :h hs)))

(defun convert-current-sprite (sprite-sheet)
  "Convert the current-animation and current-cell into an integer and store it in the sprite sheet
for later use with the shaders."
  (with-slots (%animvec %current-animation %current-cell) (animations sprite-sheet)
    (setf (sprite sprite-sheet)
          (au:href (sprites sprite-sheet)
                   (aref (aref %animvec %current-animation) (1+ %current-cell))))))

;; TODO: Shared storage is messed up semantically. We sometimes need different
;; kinds of things in shared storage with keys of differnet test functions.
(defmethod initialize-component ((sprite-sheet sprite-sheet) (context context))
  (with-slots (%spec-path %spec %material %transform %vao-id) sprite-sheet
    (let ((path (find-resource context %spec-path)))
      (setf %spec (au:safe-read-file-form path)
            ;; TODO: probably should make material id conversion to actual
            ;; material automatic instead of doing it here, but it is a very
            ;; general problem, so it needs thinking. Maybe specify a
            ;; converter function in component slots that converts it
            ;; upon load to something else? Shades of the semantic/computed
            ;; values appear...
            %material (lookup-material %material context)
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

(defmethod update-component ((sprite-sheet sprite-sheet) (context context))
  (maybe-update-current-animation-cell sprite-sheet (frame-time context)))

(defmethod render-component ((sprite-sheet sprite-sheet) (context context))

  (with-slots (%transform %material %sprite %vao-id) sprite-sheet
    (au:when-let ((camera (active-camera context)))
      (shadow:with-shader-program (shader %material)
        ;; TODO: Maybe I can merge this in to the material better--either by
        ;; making the setf uniform for materials work, or by doing something
        ;; with an ssbo that many materials use.
        (shadow:uniform-mat4 :model (fl.comp.transform:model %transform))
        (shadow:uniform-mat4 :view (fl.comp.camera:view camera))
        (shadow:uniform-mat4 :proj (fl.comp.camera:projection camera))
        ;; HACK! TODO: Update the computed-value in the material for the sprite
        ;; to render. I need to implement the setf uniform codes for materials.
        (let* ((mat-uniforms (fl.core::uniforms %material))
               (tex.sprite-value (au:href mat-uniforms :tex.sprite)))
          ;; Set the sprite uniform value in the material so we'll bind it
          ;; correctly in the next call.
          (setf (fl.core::computed-value tex.sprite-value) %sprite)

          ;; TODO: Figure out how to do this next operation such that I
          ;; personally don't have to choose a real binding point integer and
          ;; the FL or shadow system can do it for me.
          ;;
          ;; Now, lazily bind the desired interface block....
          (shadow:bind-shader-storage-block
           'fl.psilord.shaders:sprite-shader :sprite-sheet 8)
          ;; ....and the desired buffer together at the same binding point.
          (shadow:bind-buffer :sprite-sheet 8)

          ;; Normally bind the uniforms (which includes the above value) that we
          ;; currently understand like the sprite sheet texture.
          (bind-material %material)
          ;; Manually draw the empty point buffer. This kicks off the bound
          ;; shader to do its work. There are no vbo or vertex attributes bound
          ;; into this vao.
          (gl:bind-vertex-array %vao-id)
          (gl:draw-arrays :points 0 1))))))
