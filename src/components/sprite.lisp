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
            :initform 1)
   (%duration :reader duration
              :initarg :duration
              :initform 1)
   (%repeat :reader repeat
            :initarg :repeat
            :initform t)
   (%elapsed :reader elapsed
             :initform 0f0)
   (%pause :reader pause
           :initform nil))
  ((:cached-spritesheet-data equalp)))

(defclass spritesheet ()
  ((%spec :reader spec
          :initarg :spec)
   (%geometry :reader geometry
              :initarg :geometry)
   (%buffer-name :reader buffer-name)
   (%buffer-id :reader buffer-id)
   (%sprites :reader sprites
             :initform (u:dict #'equalp))))

(defun make-spritesheet (context sprite)
  (with-slots (%name %spec) sprite
    (let* ((spec (v::find-asset context %spec))
           (spritesheet (make-instance 'spritesheet
                                       :spec (u:safe-read-file-form spec)
                                       :geometry (gl:gen-vertex-array))))
      (make-spritesheet-buffer spritesheet)
      spritesheet)))

(defun write-spritesheet-buffer (spritesheet)
  (with-slots (%spec %buffer-name %sprites) spritesheet
    (loop :with buffer-name = %buffer-name
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
  (with-slots (%spec %buffer-name %buffer-id) spritesheet
    ;; TODO: This 1 is hardcoded because virality doesn't have an allocation
    ;; system for these integers.
    (gpu:bind-block :spritesheet 1)
    (setf %buffer-name (a:make-gensym :spritesheet))
    (setf %buffer-id (gpu:create-buffer %buffer-name :spritesheet))
    ;; TODO: Have materials automatically calculate a binding point instead of
    ;; hard-coding.
    (gpu:bind-buffer %buffer-name 1)
    (write-spritesheet-buffer spritesheet)))

(defun draw-sprite (sprite &optional count)
  (with-slots (%index %spritesheet) sprite
    (with-slots (%geometry) %spritesheet
      (gpu:uniform-int 'shd/sprite:sprite :sprite.index %index)
      (gl:bind-vertex-array %geometry)
      (gl:draw-arrays-instanced :triangle-strip 0 4 count)
      (gl:bind-vertex-array 0))))

(defmethod v:on-component-initialize ((self sprite))
  (with-slots (%name %spec %spritesheet %index %initial-index) self
    (let ((context (v:context self))
          (spec (a:ensure-list %spec)))
      (unless %name
        (error "A sprite component must have a name."))
      (unless spec
        (error "A sprite component must have a spritesheet spec specified."))
      (v:with-storage
          (context context)
          ((cached-spritesheet spritesheet-present-p
                               ('sprite :cached-spritesheet-data spec)
                               (make-spritesheet context self)))
        (setf %spritesheet cached-spritesheet
              %index (u:href (sprites %spritesheet) %name)
              %initial-index %index)))))

(defmethod v:on-component-update ((self sprite))
  (with-slots (%frames %index %initial-index %duration %repeat %elapsed %pause)
      self
    (unless %pause
      (incf %elapsed (v:frame-time (v:context self)))
      (if (>= %elapsed %duration)
          (setf %elapsed 0
                %index %initial-index
                %pause (unless %repeat t))
          (let* ((step (/ %elapsed %duration))
                 (min %initial-index)
                 (max (1- (+ min %frames)))
                 (index (floor (a:clamp (a:lerp step min (1+ max)) min max))))
            (setf %index index))))))
