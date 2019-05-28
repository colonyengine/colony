(in-package #:%first-light)

(defmethod load-texture-data ((texture-type (eql :texture-2d)) texture context)
  ;; TODO: This assumes no use of the general-data-descriptor or procedurally
  ;; generated content.
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data)))
    ;; load all of the images we may require.
    (let ((images (read-mipmap-images context data use-mipmaps-p :2d)))
      ;; Check to ensure they all fit into texture memory.
      ;; TODO: Refactor out of each method into validate-mipmap-images and
      ;; generalize.
      (loop :for image :across images
            :for location :across data
            :do (when (> (max (fl.image:height image) (fl.image:width image))
                         (gl:get-integer :max-texture-size))
                  (error "Image ~A for texture ~A is to big to be loaded onto ~
                          this card. Max resolution is ~A in either dimension."
                         location
                         (name texture)
                         (gl:get-integer :max-texture-size))))
      ;; Figure out the ideal mipmap count from the base resolution.
      (multiple-value-bind (expected-mipmaps expected-resolutions)
          (compute-mipmap-levels (fl.image:width (aref images 0))
                                 (fl.image:height (aref images 0)))
        (validate-mipmap-images
         images texture expected-mipmaps expected-resolutions)
        (potentially-degrade-texture-min-filter texture)
        ;; Allocate immutable storage if required.
        (when immutable-p
          (let ((num-mipmaps-to-generate
                  (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
            (%gl:tex-storage-2d texture-type num-mipmaps-to-generate
                                (fl.image:internal-format (aref images 0))
                                (fl.image:width (aref images 0))
                                (fl.image:height (aref images 0)))))
        ;; Upload all of the mipmap images into the texture ram.
        ;; TODO: Make this higher order.
        (loop :for idx :below (if use-mipmaps-p (length images) 1)
              :for level = (+ texture-base-level idx)
              :for image = (aref images idx)
              :do (with-accessors ((width fl.image:width)
                                   (height fl.image:height)
                                   (pixel-format fl.image:pixel-format)
                                   (pixel-type fl.image:pixel-type)
                                   (internal-format fl.image:internal-format)
                                   (data fl.image:data))
                      image
                    (if immutable-p
                        (gl:tex-sub-image-2d texture-type
                                             level
                                             0
                                             0
                                             width
                                             height
                                             pixel-format
                                             pixel-type
                                             data)
                        (gl:tex-image-2d texture-type
                                         level
                                         internal-format
                                         width
                                         height
                                         0
                                         pixel-format
                                         pixel-type
                                         data))))
        ;; And clean up main memory.
        ;; TODO: For procedural textures, this needs evolution.
        (free-mipmap-images images :2d)
        ;; Determine if opengl should generate the mipmaps.
        (potentially-autogenerate-mipmaps texture-type texture)))))
