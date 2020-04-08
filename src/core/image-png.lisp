(in-package #:virality.engine)

(defun get-png-channel-count (image)
  (ecase (pngload-fast:color-type image)
    (:greyscale 1)
    (:truecolour 3)
    (:truecolour-alpha 4)))

(defun get-png-pixel-format (image)
  (ecase (pngload-fast:color-type image)
    (:greyscale :red)
    (:greyscale-alpha :rg)
    (:truecolour :rgb)
    (:truecolour-alpha :rgba)))

(defun get-png-pixel-type (image)
  (ecase (pngload-fast:bit-depth image)
    (8 :unsigned-byte)
    (16 :unsigned-short)))

(defun get-png-internal-format (image)
  (let ((channel-count (get-png-channel-count image))
        (bit-depth (pngload-fast:bit-depth image)))
    (a:format-symbol :keyword "~a~d"
                     (subseq "RGBA" 0 channel-count)
                     bit-depth)))

(defmethod %load-image ((type (eql :png)) path)
  (let ((image (pngload-fast:load-file path :flatten t)))
    (make-instance 'image
                   :path path
                   :width (pngload-fast:width image)
                   :height (pngload-fast:height image)
                   :pixel-format (get-png-pixel-format image)
                   :pixel-type (get-png-pixel-type image)
                   :internal-format (get-png-internal-format image)
                   :data (pngload-fast:data image))))
