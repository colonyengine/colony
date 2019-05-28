(in-package #:%first-light)


(defmethod load-texture-data ((texture-type (eql :texture-1d-array))
                              texture context)
  (let* ((use-mipmaps-p
           (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p
           (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data))
         (num-layers (length data))
         (reshaped-layers (reshape-image-array-layout data))
         (all-layers (read-mipmap-images context
                                         reshaped-layers
                                         use-mipmaps-p
                                         :1d-array))
         (first-image (aref (aref all-layers 0) 0))
         ;; TODO: Assert num-mipmaps is same for all layers.
         (num-mipmaps (length all-layers)))
    ;; Figure out the ideal mipmap count from the base resolution.
    (multiple-value-bind (expected-mipmaps expected-resolutions)
        (compute-mipmap-levels (fl.image:width first-image)
                               (fl.image:height first-image))
      ;; TODO: Fix this call for arrays
      #++(validate-mipmap-images images texture
                                 expected-mipmaps expected-resolutions)
      (potentially-degrade-texture-min-filter texture)
      ;; Allocate storage.
      (let ((num-mipmaps-to-generate
              (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
        (if immutable-p
            (%gl:tex-storage-2d texture-type
                                num-mipmaps-to-generate
                                (fl.image:internal-format first-image)
                                (fl.image:width first-image)
                                num-layers)
            (loop :for i :below num-mipmaps-to-generate
                  :for mipmap-resolution :in expected-resolutions
                  :do (gl:tex-image-2d texture-type
                                       (+ texture-base-level i)
                                       (fl.image:internal-format first-image)
                                       (first mipmap-resolution)
                                       num-layers
                                       0
                                       (fl.image:pixel-format first-image)
                                       (fl.image:pixel-type first-image)
                                       (cffi:null-pointer)))))
      ;; Upload all of the mipmap images into the texture ram.
      ;; TODO: Make this higher order.
      (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
            :for level = (+ texture-base-level idx)
            :for image = (aref (aref all-layers idx) 0)
            ;; Construct the entire 2d array image of these 1d image pieces.
            :do (with-accessors ((width fl.image:width)
                                 (height fl.image:height)
                                 (internal-format fl.image:internal-format)
                                 (pixel-format fl.image:pixel-format)
                                 (pixel-type fl.image:pixel-type))
                    image
                  (dotimes (l num-layers)
                    (gl:tex-sub-image-2d texture-type
                                         level
                                         0
                                         l
                                         width
                                         1
                                         pixel-format
                                         pixel-type
                                         (fl.image:data
                                          (aref (aref all-layers idx) l))))))
      ;; And clean up main memory.
      (free-mipmap-images all-layers :1d-array)
      ;; Determine if opengl should generate the mipmaps.
      (potentially-autogenerate-mipmaps texture-type texture))))
