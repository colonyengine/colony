(in-package :first-light.image-types)

(defclass image ()
  ((%type :reader image-type
          :initarg :type)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%depth :reader depth
           :initarg :depth)
   (%origin :reader origin
            :initarg :origin
            :initform :top-left)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%pixel-type :reader pixel-type
                :initarg :pixel-type
                :initform :unsigned-byte)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%data :reader data
          :initarg :data)
   (%metadata :reader metadata
              :initform (fl.util:dict #'eq))))

(defun make-image (&rest init-args)
  (apply #'make-instance 'image init-args))

(defun get-image-extension-keyword (path)
  (fl.util:make-keyword (string-upcase (pathname-type path))))

(defun get-image-type (path)
  (let ((extension (get-image-extension-keyword path)))
    (ecase extension
      (:tga :targa)
      (:png :png)
      ((:jpg :jpeg) :jpeg))))

(defmethod free-storage ((image image))
  (with-slots (%data) image
    (setf %data nil)))

(defmethod get-pixel-size ((image image))
  "Return the size of the pixel element in this image in bytes."
  (ecase (pixel-format image)
    (:red 1)
    ((:rgb :bgr) 3)
    ((:rgba :bgra) 4)))

(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:red :r8)
    ((:rgb :bgr) :rgb8)
    ((:rgba :bgra) :rgba8)))

(defun read-image (path)
  (read-image-type (get-image-type path) path))
