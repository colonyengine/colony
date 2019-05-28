(in-package #:first-light.image-types)

(defun get-surface-channel-count (surface)
  (let ((format (sdl2:surface-format-format surface)))
    (ecase format
      ((:index8)
       1)
      ((:rgb24 :bgr24 :rgb888 :bgr888)
       3)
      ((:argb8888 :rgba8888 :abgr8888 :bgra8888 :rgba32 :argb32 :bgra32 :abgr32)
       4))))

(defun get-surface-pixel-format (surface)
  (let ((format (sdl2:surface-format-format surface)))
    (ecase format
      (:index8 :red)
      ((:rgb24 :rgb888)
       :rgb)
      ((:bgr24 :bgr888)
       :bgr)
      ((:rgba8888 :abgr8888 :rgba32 :abgr32)
       :rgba)
      ((:argb8888 :bgra8888 :argb32 :bgra32)
       :bgra))))

(defmethod %read-image ((loader (eql :sdl2-image)) path)
  (let* ((surface (sdl2-image:load-image path))
         (pixel-format (get-surface-pixel-format surface))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface))
         (channels (get-surface-channel-count surface)))
    (make-image :path path
                :type (get-image-extension-keyword path)
                :surface surface
                :width width
                :height height
                :channels channels
                :pixel-format pixel-format
                :internal-format (get-internal-format pixel-format)
                :data (sdl2:surface-pixels surface))))

(defmethod %free-storage ((loader (eql :sdl2-image)) image)
  (with-slots (%surface %data) image
    (sdl2:free-surface %surface)
    (setf %surface nil
          %data nil)))
