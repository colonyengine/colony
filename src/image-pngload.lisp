(in-package #:virality.image)

(defmethod get-image-channel-count ((loader (eql :pngload)) raw-data)
  (ecase (pngload:color-type raw-data)
    (:greyscale 1)
    (:truecolour 3)
    (:truecolour-alpha 4)))

(defmethod get-image-pixel-format ((loader (eql :pngload)) raw-data)
  (ecase (pngload:color-type raw-data)
    (:greyscale :red)
    (:truecolour :rgb)
    (:truecolour-alpha :rgba)))

(defmethod %read-image ((loader (eql :pngload)) path)
  (let* ((raw-data (pngload:load-file path :flatten t :flip-y t))
         (pixel-format (get-image-pixel-format loader raw-data)))
    (make-instance 'image
                   :path path
                   :type (get-image-extension-keyword path)
                   :raw-data raw-data
                   :width (pngload:width raw-data)
                   :height (pngload:height raw-data)
                   :channels (get-image-channel-count loader raw-data)
                   :pixel-format pixel-format
                   :internal-format (get-internal-format pixel-format)
                   :data (pngload:data raw-data))))
