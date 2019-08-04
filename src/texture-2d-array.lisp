(in-package #:virality.engine)

(defmethod load-texture-data ((texture-type (eql :texture-2d-array))
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
         (all-layers
           (read-mipmap-images
            context reshaped-layers use-mipmaps-p :2d-array))
         (first-image (aref (aref all-layers 0) 0))
         ;; TODO: Assert num-mipmaps is same for all layers.
         (num-mipmaps (length all-layers)))
    ;; Figure out the ideal mipmap count from the base resolution.
    (multiple-value-bind (expected-mipmaps expected-resolutions)
        (compute-mipmap-levels (img:width first-image)
                               (img:height first-image))
      ;; TODO: Fix this call for arrays
      #++(validate-mipmap-images images texture
                                 expected-mipmaps expected-resolutions)
      (potentially-degrade-texture-min-filter texture)
      ;; Allocate storage.
      (let ((num-mipmaps-to-generate
              (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
        (if immutable-p
            (%gl:tex-storage-3d texture-type
                                num-mipmaps-to-generate
                                (img:internal-format first-image)
                                (img:width first-image)
                                (img:height first-image)
                                num-layers)
            (loop :for i :below num-mipmaps-to-generate
                  :for (mipmap-width mipmap-height) :in expected-resolutions
                  :do (gl:tex-image-3d texture-type
                                       (+ texture-base-level i)
                                       (img:internal-format first-image)
                                       mipmap-width
                                       mipmap-height
                                       num-layers
                                       0
                                       (img:pixel-format first-image)
                                       (img:pixel-type first-image)
                                       (cffi:null-pointer)))))
      ;; Upload all of the mipmap images into the texture ram.
      ;; TODO: Make this higher order.
      (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
            :for level = (+ texture-base-level idx)
            :for image = (aref (aref all-layers idx) 0)
            ;; Construct the entire 2d array image of these 1d image pieces.
            :do (dotimes (i num-layers)
                  (gl:tex-sub-image-3d
                   texture-type
                   level
                   0
                   0
                   i
                   (img:width image)
                   (img:height image)
                   1
                   (img:pixel-format image)
                   (img:pixel-type image)
                   (img:data (aref (aref all-layers idx) i)))))
      ;; And clean up main memory.
      (free-mipmap-images all-layers :2d-array)
      ;; Determine if opengl should generate the mipmaps.
      (potentially-autogenerate-mipmaps texture-type texture))))
