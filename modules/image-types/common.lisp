(in-package :first-light.image-types)

(defclass image ()
  ((%path :reader path
          :initarg :path)
   (%type :reader image-type
          :initarg :type)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%channels :reader channels
              :initarg :channels)
   (%depth :reader depth
           :initarg :depth)
   (%origin :reader origin
            :initarg :origin)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%pixel-type :reader pixel-type
                :initarg :pixel-type
                :initform :unsigned-byte)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%data :reader data
          :initarg :data)))

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
  (/ (* (depth image) (channels image)) 8))

(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:red :r8)
    ((:rgb :bgr) :rgb8)
    ((:rgba :bgra) :rgba8)))

(defun read-image (path)
  (let ((image (read-image-type (get-image-type path) path)))
    (unless (eq (origin image) :top-left)
      (v:warn :fl.image "Image origin is not :TOP-LEFT for file: ~s" path))
    image))
