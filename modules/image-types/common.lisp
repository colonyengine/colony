(in-package :first-light.image-types)

(defclass image ()
  ((%path :reader path
          :initarg :path)
   (%type :reader image-type
          :initarg :type)
   (%surface :reader surface
             :initarg :surface)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%channels :reader channels
              :initarg :channels)
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
  (u:make-keyword (string-upcase (pathname-type path))))

(defun get-loader-type (path)
  (let ((extension (get-image-extension-keyword path)))
    (ecase extension
      ((:tga :bmp :pbm :pgm :ppm :xpm :xcf :pcx :gif :jpg :jpeg :tif :tiff :lbm :iff :png)
       :sdl2-image))))

(defmethod get-pixel-size ((image image))
  (ecase (pixel-format image)
    (:red 1)
    ((:rgb :bgr) 3)
    ((:rgba :bgra) 4)))

(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:red :r8)
    ((:rgb :bgr) :rgb8)
    ((:rgba :bgra) :rgba8)))

(defgeneric %read-image (loader path))

(defun read-image (path)
  (%read-image (get-loader-type path) path))

(defun free-storage (image)
  (%free-storage (get-loader-type (path image)) image))
