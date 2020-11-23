(in-package #:virality)

(defclass image ()
  ((%path :reader path
          :initarg :path
          :initform nil)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%pixel-type :reader pixel-type
                :initarg :pixel-type)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%data :reader data
          :initarg :data
          :initform nil)))

(defun get-image-type (path)
  (u:make-keyword (string-upcase (pathname-type path))))

(defgeneric %load-image (type path &key)
  (:method (type path &key)
    (error "Unsupported image type ~s for asset: ~s." type path)))

(defgeneric load-image (path &key flip-y &allow-other-keys))

;; This function is not used yet, but will be when we have framebuffer support.
(defmethod load-image ((path null)
                       &key width height pixel-format pixel-type
                         internal-format)
  (make-instance 'image
                 :width width
                 :height height
                 :pixel-format pixel-format
                 :pixel-type pixel-type
                 :internal-format internal-format))

(defmethod load-image (path &key flip-y)
  (let ((type (get-image-type path)))
    (%load-image type path :flip-y flip-y)))
