(in-package :first-light.image-types)

(defun parse-targa-origin (path)
  (fl.util:with-binary-input (in path)
    (fl.binfmt:with-buffer-read (:stream in)
      (fl.binfmt:read-bytes 8)
      (let ((origin-x (fl.binfmt:read-int-be 2))
            (origin-y (fl.binfmt:read-int-be 2)))
        (cond
          ((and (zerop origin-x) (plusp origin-y))
           :top-left)
          ((and (zerop origin-x) (zerop origin-y))
           :bottom-left)
          ((and (plusp origin-x) (plusp origin-y))
           :top-right)
          ((and (plusp origin-x) (zerop origin-y))
           :bottom-right))))))

(defmethod read-image-type ((type (eql :targa)) path)
  (flet ((get-pixel-format (channels)
           (ecase channels
             (1 :red)
             (3 :bgr)
             (4 :bgra))))
    (let* ((image (tga:read-tga path))
           (pixel-format (get-pixel-format (tga:image-channels image)))
           (origin (parse-targa-origin path)))
      (unless (eq origin :top-left)
        (v:warn :fl.image "Image origin is not :TOP-LEFT for file: ~s" path))
      (make-image :type type
                  :width (tga:image-width image)
                  :height (tga:image-height image)
                  :depth (tga:image-bpp image)
                  :pixel-format pixel-format
                  :internal-format (get-internal-format pixel-format)
                  :origin origin
                  :data (tga:image-data image)))))
