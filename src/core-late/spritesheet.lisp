(in-package #:virality)

(defclass spritesheet ()
  ((%name :reader name
          :initarg :name)
   (%spec :reader spec
          :initarg :spec)
   (%geometry :reader geometry
              :initarg :geometry)
   (%sprites :reader sprites
             :initform (u:dict #'equalp))))

(defun make-spritesheet-buffer (spritesheet)
  (with-slots (%name) spritesheet
    ;; TODO: This 1 is hardcoded because virality doesn't have an allocation
    ;; system for these integers.
    (shadow:bind-block :spritesheet 1)
    (shadow:create-buffer %name :spritesheet)
    ;; TODO: Have materials automatically calculate a binding point instead of
    ;; hard-coding.
    (shadow:bind-buffer %name 1)))

(defun update-spritesheet-buffer (spritesheet)
  (loop :with name = (name spritesheet)
        :with spec = (spec spritesheet)
        :with count = (length spec)
        :with pos = (make-array count)
        :with size = (make-array count)
        :for sprite :in spec
        :for i :from 0
        :do (destructuring-bind (&key id x y w h y-flipped &allow-other-keys)
                sprite
              (when (and id x y w h)
                ;; TODO: Make sure y-flip math is correct
                (setf (aref pos i) (vector x (if y-flipped (- 1 y h) y))
                      (aref size i) (vector w h)
                      (u:href (sprites spritesheet) id) i)))
        :finally (shadow:write-buffer-path name :pos pos)
                 (shadow:write-buffer-path name :size size)))

(defun find-sprite (spritesheet name)
  (or (u:href (sprites spritesheet) name)
      (error "Sprite ~s not found in spritesheet ~s."
             name
             (name spritesheet))))

(defun make-spritesheet (context spec)
  (let ((path (v::resolve-path spec)))
    (v:with-asset-cache context :spritesheet spec
      (let ((spritesheet (make-instance 'spritesheet
                                        :name spec
                                        :spec (u:safe-read-file-form path)
                                        :geometry (gl:gen-vertex-array))))
        (make-spritesheet-buffer spritesheet)
        (update-spritesheet-buffer spritesheet)
        spritesheet))))
