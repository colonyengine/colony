(in-package #:colony.texture)

(defmethod load-texture-data ((texture-type (eql :texture-1d-array))
                              texture context)
  ;; TODO: This assumes no use of the general-data-descriptor or procedurally
  ;; generated content.
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level)))

    ;; load all of the images we may require.
    ;;
    ;; TODO: Reading the data here is an interim solution. Move to texture-map.
    ;; Note we get back the _reshaped_ images array.
    (let* ((images (read-texture-mipmaps texture :1d-array context))
           (num-layers (length (aref images 0)))
           (num-mipmaps (length images)))

      ;; TODO: We have to read at least the headers of the images to get their
      ;; size and internal-format so we can allocate the right amount of space
      ;; on the GPU for the data of this texture.
      (gpu-allocate-texture-storage texture-type texture images)

      ;; Upload all of the mipmap image layers into the texture ram.
      ;; TODO: Allow scheduling of this work across frames.
      (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
            :for level = (+ texture-base-level idx)
            ;; COnstruct the entire 2d array image of these image pieces.
            :do (dotimes (i num-layers)
                  (let ((image (aref (aref images idx) i)))
                    (gpu-upload-texture-mipmap-layer
                     texture-type texture image level :offset i))))

      ;; Determine if opengl should generate the mipmaps.
      (potentially-autogenerate-mipmaps texture-type texture))))

(defmethod read-texture-mipmaps (texture (kind (eql :1d-array)) context)
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (flip-y (get-computed-applied-attribute texture :flip-y))
         (data (get-computed-applied-attribute texture :data))
         (reshaped-layers (reshape-image-array-layout data)))

    ;; load all of the images we may require.
    ;;
    ;; TODO: Move the loading of the image data into main memory to texture-map
    ;; loading w/ the resource-cache, etc. This texture object should just have
    ;; a reference to the texture-map object holding the data.
    (let ((images (read-mipmap-images
                   context reshaped-layers use-mipmaps-p :1d-array flip-y)))
      ;; TODO: Check to ensure they all fit into texture memory.

      ;; The reshaped array of images which are the mipmaps.
      images)))

(defmethod gpu-allocate-texture-storage ((texture-type (eql :texture-1d-array))
                                         texture images)
  ;; NOTE: This requires knowing the actual size of the level 0 texture AND the
  ;; base/max mipmap level AND the internal format in order to compute the
  ;; storage properly.

  ;; NOTE: When the texture is mutable, all texture mipmap layers must be
  ;; allocated individually (and have their internal formats and etc all be
  ;; consistent. When immutable, they can be allocated all at once in a much
  ;; easier fashion.

  (let* ((immutable-p
           (get-computed-applied-attribute texture :immutable))
         (use-mipmaps-p
           (get-computed-applied-attribute texture :use-mipmaps))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps
           (- texture-max-level texture-base-level))
         (num-layers (length (aref images 0)))
         (first-image (aref (aref images 0) 0)))

    ;; TODO: Move the computing of the mipmap levels to the texture-map codes.
    (multiple-value-bind (expected-mipmaps expected-resolutions)
        (compute-mipmap-levels (img:width first-image)
                               (img:height first-image))

      ;; TODO Fix this call for arrays.
      #++(validate-mipmap-images
          images texture expected-mipmaps expected-resolutions)

      ;; Allocate storage for the texture.
      (let ((num-mipmaps-to-generate (if use-mipmaps-p
                                         (min expected-mipmaps max-mipmaps)
                                         1)))
        (if immutable-p
            ;; We can allocate the storage for the complete texture
            ;; all at once.
            (%gl:tex-storage-2d texture-type
                                num-mipmaps-to-generate
                                (img:internal-format first-image)
                                (img:width first-image)
                                num-layers)

            ;; TODO: I don't think this code is actually correct. It seems like
            ;; we would need a loop nested in another loop. Validate it.
            ;;
            ;; We must allocate the complete texture layers individually.
            ;; TODO: We need to validate the internal-format and all that
            ;; are correct. The texture-map codes should do this.
            (loop :for i :below num-mipmaps-to-generate
                  :for mipmap-resolution :in expected-resolutions
                  :do (gl:tex-image-2d texture-type
                                       (+ texture-base-level i)
                                       (img:internal-format first-image)
                                       (first mipmap-resolution)
                                       num-layers
                                       0
                                       (img:pixel-format first-image)
                                       (img:pixel-type first-image)
                                       (cffi:null-pointer))))))))

(defmethod gpu-upload-texture-mipmap-layer ((texture-type
                                             (eql :texture-1d-array))
                                            texture image level
                                            &key offset face-signifier)
  (assert (integerp offset))
  (assert (null face-signifier))
  (gl:tex-sub-image-2d texture-type
                       level
                       0
                       offset
                       (img:width image)
                       1
                       (img:pixel-format image)
                       (img:pixel-type image)
                       (img:data image)))
