(in-package :fl.mfiano)

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
  ((transform :default nil)
   (spec-path :default nil)
   (spec :default nil)
   (material :default nil)
   (vao-id :default nil)
   (vbo-id :default nil)
   (sprite :default nil)
   (sprites :default (au:dict #'equalp))
   (animations :default nil)))

(defun make-sprite-sheet-buffer (sprite-sheet)

  (unless (shadow:find-block :sprite-sheet-block)
    (shadow::create-block-alias :buffer
                                :sprite-sheet
                                'fl.mfiano.shaders:sprite-test
                                :sprite-sheet-block))
  (shadow:bind-block :sprite-sheet-block 8)
  (shadow:create-buffer :sprite-sheet-buffer :sprite-sheet-block)
  (shadow:bind-buffer :sprite-sheet-buffer 8)

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
        :finally (shadow:write-buffer-path :sprite-sheet-buffer :x xs)
                 (shadow:write-buffer-path :sprite-sheet-buffer :y ys)
                 (shadow:write-buffer-path :sprite-sheet-buffer :w ws)
                 (shadow:write-buffer-path :sprite-sheet-buffer :h hs)))

(defun convert-current-sprite (sprite-sheet)
  "Convert the current-animation and current-cell into an integer and store it in the sprite sheet
for later use with the shaders."
  (with-slots (%animvec %current-animation %current-cell) (animations sprite-sheet)
    (setf (sprite sprite-sheet)
          (au:href (sprites sprite-sheet)
                   (aref (aref %animvec %current-animation) (1+ %current-cell))))))

(defmethod initialize-component ((sprite-sheet sprite-sheet) (context context))
  (with-slots (%spec-path %spec %material %transform %vao-id) sprite-sheet
    (let ((path (find-resource context %spec-path)))
      (setf %spec (au:safe-read-file-form path)
            %material (lookup-material %material context)
            %transform (actor-component-by-type (actor sprite-sheet) 'transform)
            %vao-id (gl:gen-vertex-array))
      (make-sprite-sheet-buffer sprite-sheet)
      (convert-current-sprite sprite-sheet))))

(defun maybe-update-current-animation-cell (sprite-sheet frame-time)
  (with-slots (%elapsed-time %cell-time %current-animation %current-cell %animvec)
      (animations sprite-sheet)
    (incf %elapsed-time frame-time)
    (when (>= %elapsed-time %cell-time)
      (loop :until (< %elapsed-time %cell-time)
            :do (setf %current-cell
                      (mod (1+ %current-cell)
                           (1- (length (aref %animvec %current-animation)))))
                (decf %elapsed-time %cell-time)
                (when (zerop %elapsed-time)
                  (setf %elapsed-time 0)))
      (convert-current-sprite sprite-sheet))))

(defmethod update-component ((sprite-sheet sprite-sheet) (context context))
  (maybe-update-current-animation-cell sprite-sheet (frame-time context))
  (fl.comp.transform::translate (transform sprite-sheet) :local (v3:make 0 0.01 0)))

(defmethod render-component ((sprite-sheet sprite-sheet) (context context))
  (with-slots (%transform %material %sprite %vao-id) sprite-sheet
    (au:when-let ((camera (active-camera context)))
      (shadow:with-shader-program (shader %material)
        (setf (mat-uniform-ref %material :model) (fl.comp.transform:model %transform)
              (mat-uniform-ref %material :view) (fl.comp.camera:view camera)
              (mat-uniform-ref %material :proj) (fl.comp.camera:projection camera)
              (mat-uniform-ref %material :tex.sprite) %sprite)
        (bind-material %material)
        (gl:bind-vertex-array %vao-id)
        (gl:draw-arrays :points 0 1)))))
