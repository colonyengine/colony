(in-package :%fl)

(defclass image ()
  ((%width :reader width
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
          :initarg :data)))

(defun make-image (&rest init-args)
  (apply #'make-instance 'image init-args))

(defmethod free-storage ((image image))
  (with-slots (%data) image
    ;; the tga codes will auto clean up
    (setf %data nil)))

(defmethod get-pixel-size ((image image))
  "Return the size of the pixel element in this image in bytes."
  (ecase (pixel-format image)
    (:red 1)
    (:bgr 3)
    (:bgra 4)))

;; specific to TGA
(defun get-pixel-format (channels)
  (ecase channels
    (1 :red)
    (3 :bgr)
    (4 :bgra)))

;; Specific to TGA
(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:red :r8)
    (:bgr :rgb8)
    (:bgra :rgba8)))

;; TODO: Currently, this only reads TGA files. We can abstract it later to
;; different image files if it becomes important.
(defun read-image (context location)
  (let* ((path (apply #'find-resource context (fu:ensure-list location)))
         (image (tga:read-tga path))
         (pixel-format (get-pixel-format (tga:image-channels image))))
    (make-image :width (tga:image-width image)
                :height (tga:image-height image)
                :pixel-format pixel-format
                :pixel-type :unsigned-byte
                :internal-format (get-internal-format pixel-format)
                :data (tga:image-data image))))
