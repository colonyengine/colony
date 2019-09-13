(in-package #:virality.image)

(defmethod get-image-channel-count ((loader (eql :sdl2-image)) image)
  (ecase (sdl2:surface-format-format image)
    (:index8
     1)
    ((:rgb24 :bgr24 :rgb888 :bgr888)
     3)
    ((:argb8888 :rgba8888 :abgr8888 :bgra8888 :rgba32 :argb32 :bgra32 :abgr32)
     4)))

(defmethod get-image-pixel-format ((loader (eql :sdl2-image)) image)
  (ecase (sdl2:surface-format-format image)
    (:index8 :red)
    ((:rgb24 :rgb888)
     :rgb)
    ((:bgr24 :bgr888)
     :bgr)
    ((:rgba8888 :abgr8888 :rgba32 :abgr32)
     :rgba)
    ((:argb8888 :bgra8888 :argb32 :bgra32)
     :bgra)))

(defmethod %read-image ((loader (eql :sdl2-image)) path)
  (let* ((raw-data (sdl2-image:load-image path))
         (pixel-format (get-image-pixel-format loader raw-data))
         (width (sdl2:surface-width raw-data))
         (height (sdl2:surface-height raw-data))
         (channels (get-image-channel-count loader raw-data)))
    (make-instance 'image
                   :path path
                   :type (get-image-extension-keyword path)
                   :raw-data raw-data
                   :width width
                   :height height
                   :channels channels
                   :pixel-format pixel-format
                   :internal-format (get-internal-format pixel-format)
                   :data (sdl2:surface-pixels raw-data))))

(defmethod %free-storage ((loader (eql :sdl2-image)) image)
  (with-slots (%raw-data %data) image
    (sdl2:free-surface %raw-data)))
