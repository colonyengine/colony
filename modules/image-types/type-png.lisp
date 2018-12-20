(in-package :first-light.image-types)

(defmethod read-image-type ((type (eql :png)) path)
  (flet ((get-pixel-data (color-type)
           (ecase color-type
             (:greyscale (values 1 :red))
             (:truecolour (values 3 :rgb))
             (:truecolour-alpha (values 4 :rgba)))))
    (let ((image (pngload:load-file path :flatten t)))
      (fl.util:mvlet ((channels pixel-format (get-pixel-data (pngload:color-type image))))
        (make-image :path path
                    :type type
                    :width (pngload:width image)
                    :height (pngload:height image)
                    :channels channels
                    :depth (pngload:bit-depth image)
                    :origin :top-left
                    :pixel-format pixel-format
                    :internal-format (get-internal-format pixel-format)
                    :data (pngload:data image))))))
