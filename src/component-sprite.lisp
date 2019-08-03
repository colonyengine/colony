(in-package #:virality.components)

(v:define-component sprite ()
  ((%name :accessor name
          :initarg :name
          :initform nil)
   (%spec :reader spec
          :initarg :spec
          :initform nil)
   (%spritesheet :reader spritesheet)
   (%index :reader index)
   (%initial-index :reader initial-index)
   (%frames :accessor frames
            :initarg :frames
            :initform 1))
  ((:cached-spritesheet-data equalp)))

(defclass spritesheet ()
  ((%spec :reader spec
          :initarg :spec)
   (%geometry :reader geometry
              :initarg :geometry)
   (%buffer :reader buffer)
   (%sprites :reader sprites
             :initform (u:dict #'equalp))))

(defun make-spritesheet (context sprite)
  (with-slots (%name %spec) sprite
    (let* ((spec (v::find-resource context %spec))
           (spritesheet (make-instance 'spritesheet
                                       :spec (u:safe-read-file-form spec)
                                       :geometry (gl:gen-vertex-array))))
      (make-spritesheet-buffer spritesheet)
      spritesheet)))

(defun write-spritesheet-buffer (spritesheet)
  (with-slots (%spec %buffer %sprites) spritesheet
    (loop :with buffer-name = (gpu:buffer-name %buffer)
          :with count = (length %spec)
          :with pos = (make-array count)
          :with size = (make-array count)
          :for sprite :in %spec
          :for i :from 0
          :do (destructuring-bind (&key id x y w h) sprite
                (when (and id x y w h)
                  (setf (aref pos i) (v2:vec x y)
                        (aref size i) (v2:vec w h)
                        (u:href %sprites id) i)))
          :finally (gpu:write-buffer-path buffer-name :pos pos)
                   (gpu:write-buffer-path buffer-name :size size))))

(defun make-spritesheet-buffer (spritesheet)
  (with-slots (%spec %buffer) spritesheet
    (gpu:bind-block :spritesheet 1)
    (setf %buffer (gpu:create-buffer (a:make-gensym :spritesheet)
                                     :spritesheet))
    (gpu:bind-buffer (gpu:buffer-name %buffer) 1)
    (write-spritesheet-buffer spritesheet)))

(defun update-sprite-index (sprite step)
  (with-slots (%index %initial-index %frames) sprite
    (let ((max (1- (+ %initial-index %frames))))
      (setf %index (floor (u:map-domain 0 1 %initial-index max step))))))

(defun draw-sprite (sprite &optional count)
  (with-slots (%index %spritesheet) sprite
    (with-slots (%geometry) %spritesheet
      (gpu:uniform-int 'first-light.shader.sprite:sprite :sprite.index %index)
      (gl:bind-vertex-array %geometry)
      (gl:draw-arrays-instanced :points 0 1 count)
      (gl:bind-vertex-array 0))))

(defmethod v:on-component-initialize ((self sprite))
  (with-slots (%spritesheet %index %initial-index) self
    (let ((context (v:context self))
          (name (name self))
          (spec (a:ensure-list (spec self))))
      (unless name
        (error "A sprite component must have a name."))
      (unless spec
        (error "A sprite component must have a spritesheet spec specified."))
      (v:with-shared-storage
          (context context)
          ((cached-spritesheet spritesheet-present-p
                               ('sprite :cached-spritesheet-data spec)
                               (make-spritesheet context self)))
        (setf %spritesheet cached-spritesheet
              %index (u:href (sprites %spritesheet) name)
              %initial-index %index)))))
