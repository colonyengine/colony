(in-package :first-light.image-types)

(defun parse-targa-origin (path)
  (fl.util:with-binary-input (in path)
    (fl.binfmt:with-buffer-read (:stream in)
      (fl.binfmt:read-bits 140)
      (aref #(:bottom-left :bottom-right :top-left :top-right)
            (fl.binfmt:read-bits 2)))))

(defmethod read-image-type ((type (eql :targa)) path)
  (flet ((get-pixel-format (channels)
           (ecase channels
             (1 :red)
             (3 :bgr)
             (4 :bgra))))
    (let* ((image (tga:read-tga path))
           (pixel-format (get-pixel-format (tga:image-channels image)))
           (channels (tga:image-channels image)))
      (make-image :path path
                  :type type
                  :width (tga:image-width image)
                  :height (tga:image-height image)
                  :channels channels
                  :depth (floor (tga:image-bpp image) channels)
                  :origin (parse-targa-origin path)
                  :pixel-format pixel-format
                  :internal-format (get-internal-format pixel-format)
                  :data (tga:image-data image)))))
