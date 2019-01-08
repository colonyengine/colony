(in-package :first-light.components)

(define-component sprite ()
  ((name :default nil)
   (spec :default nil)
   (spritesheet :default nil)
   (index :default nil)
   (initial-index :default nil)
   (frames :default 1))
  ((:cached-spritesheet-data equalp)))

(defclass spritesheet ()
  ((%spec :reader spec
          :initarg :spec)
   (%geometry :reader geometry
              :initarg :geometry)
   (%buffer :reader buffer)
   (%sprites :reader sprites
             :initform (fl.util:dict #'equalp))))

(defun make-spritesheet (context sprite)
  (with-slots (%name %spec) sprite
    (let* ((spec (find-resource context %spec))
           (spritesheet (make-instance 'spritesheet
                                       :spec (fl.util:safe-read-file-form spec)
                                       :geometry (gl:gen-vertex-array))))
      (make-spritesheet-buffer spritesheet)
      spritesheet)))

(defun write-spritesheet-buffer (spritesheet)
  (with-slots (%spec %buffer %sprites) spritesheet
    (loop :with buffer-name = (fl.gpu:buffer-name %buffer)
          :with count = (length %spec)
          :with pos = (make-array count)
          :with size = (make-array count)
          :for sprite :in %spec
          :for i :from 0
          :do (destructuring-bind (&key id x y w h) sprite
                (when (and id x y w h)
                  (setf (aref pos i) (flm:vec2 x y)
                        (aref size i) (flm:vec2 w h)
                        (fl.util:href %sprites id) i)))
          :finally (fl.gpu:write-buffer-path buffer-name :pos pos)
                   (fl.gpu:write-buffer-path buffer-name :size size))))

(defun make-spritesheet-buffer (spritesheet)
  (with-slots (%spec %buffer) spritesheet
    (fl.gpu:bind-block :spritesheet 1)
    (setf %buffer (fl.gpu:create-buffer (fl.util:unique-name :spritesheet) :spritesheet))
    (fl.gpu:bind-buffer (fl.gpu:buffer-name %buffer) 1)
    (write-spritesheet-buffer spritesheet)))

(defun draw-sprite (sprite &optional count)
  (with-slots (%index %spritesheet) sprite
    (with-slots (%geometry) %spritesheet
      (fl.gpu:uniform-int 'fl.gpu.sprite:sprite :sprite.index %index)
      (gl:bind-vertex-array %geometry)
      (gl:draw-arrays-instanced :points 0 1 count)
      (gl:bind-vertex-array 0))))

(defmethod on-component-initialize ((self sprite))
  (with-accessors ((context context) (name name) (spec spec) (spritesheet spritesheet)
                   (index index) (initial-index initial-index))
      self
    (unless name
      (error "A sprite component must have a name."))
    (unless spec
      (error "A sprite component must have a spritesheet spec specified."))
    (let ((spec (fl.util:ensure-list spec)))
      (with-shared-storage
          (context context)
          ((cached-spritesheet spritesheet-present-p
                               ('sprite :cached-spritesheet-data spec)
                               (make-spritesheet context self)))
        (setf spritesheet cached-spritesheet
              index (fl.util:href (sprites spritesheet) name)
              initial-index index)))))
