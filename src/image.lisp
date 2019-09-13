(in-package #:virality.image)

(defclass image ()
  ((%path :reader path
          :initarg :path)
   (%type :reader image-type
          :initarg :type)
   (%raw-data :reader raw-data
              :initarg :raw-data)
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

;;; Generic functions all loaders should implement

(defgeneric get-image-channel-count (loader image))

(defgeneric get-image-pixel-format (loader image))

(defgeneric %read-image (loader path))

(defgeneric %free-storage (loader image)
  (:method (loader image))
  (:method :after (loader image)
    (with-slots (%raw-data %data) image
      (setf %raw-data nil
            %data nil)
      (u:noop))))

;;; Internals

(defun get-image-extension-keyword (path)
  (u:make-keyword (string-upcase (pathname-type path))))

(defun get-loader-type (path)
  (ecase (get-image-extension-keyword path)
    (:png
     :pngload)
    ((:tga :bmp :pbm :pgm :ppm :xpm :xcf :pcx :gif :jpg :jpeg :tif :tiff :lbm
      :iff)
     :sdl2-image)))

(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:red :r8)
    ((:rgb :bgr) :rgb8)
    ((:rgba :bgra) :rgba8)))

;;; Engine API

(defun read-image (path)
  (%read-image (get-loader-type path) path))

(defun free-storage (image)
  (%free-storage (get-loader-type (path image)) image))
