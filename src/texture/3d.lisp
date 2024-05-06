(in-package #:colony.texture)

(defmethod load-texture-data ((texture-type (eql :texture-3d)) texture context)
  ;; Determine if loading :images or :volume
  ;; TODO: Validating a 3d texture.
  ;; 0. Ensure that each mipmap level has the same number of slices.
  ;; 1. Ensure that all mipmap levels have the same number of slices.
  ;; 2. Ensure that all required mipmaps levels are present and correct.
  ;; 2. Ensure that each mipmap level fits into the current limits on the card.
  ;; TODO: Fix me. Need to support :layout properly. Currently, this form
  ;; represents opengl defaults.

  ;; TODO: This assumes no use of the general-data-descriptor or procedurally
  ;; generated content.
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level)))

    ;; load all of the images we may require.
    ;;
    ;; TODO: Reading the data here is an interim solution. Move to texture-map.
    (let* ((images (read-texture-mipmaps texture :3d context))
           (num-mipmaps (length images)))

      ;; TODO: We have to read at least the headers of the images to get their
      ;; size and internal-format so we can allocate the right amount of space
      ;; on the GPU for the data of this texture.
      (gpu-allocate-texture-storage texture-type texture images)

      ;; Upload all of the mipmap image layers into the texture ram.
      ;; TODO: Allow scheduling of this work across frames.
      (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
            :for level = (+ texture-base-level idx)
            :do (dotimes (i (length (aref images idx)))

                  (let ((sub-image (aref (aref images idx) i)))
                    (gpu-upload-texture-mipmap-layer
                     texture-type
                     texture
                     sub-image
                     level
                     :offset i))))
      ;; Determine if opengl should generate the mipmaps.
      (potentially-autogenerate-mipmaps texture-type texture))))

(defmethod read-texture-mipmaps (texture (kind (eql :3d)) context)
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (data (get-computed-applied-attribute texture :data))
         (flip-y (get-computed-applied-attribute texture :flip-y)))

    ;; load all of the images we may require.
    ;;
    ;; TODO: Move the loading of the image data into main memory to texture-map
    ;; loading w/ the resource-cache, etc. This texture object should just have
    ;; a reference to the texture-map object holding the data.
    (let ((images (read-mipmap-images context data use-mipmaps-p :3d flip-y)))
      ;; TODO: Check to ensure they all fit into texture memory.

      ;; The array of images which re the mipmaps.
      images)))

(defmethod gpu-allocate-texture-storage ((texture-type (eql :texture-3d))
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
         (max-texture-3d-size (c::get-gpu-parameter :max-3d-texture-size))
         (first-image (aref (aref images 0) 0))
         (depth (length (aref images 0))))
    ;; TODO: Move the computing of the mipmap levels to the texture-map codes.
    (multiple-value-bind (expected-mipmaps expected-resolutions)
        (compute-mipmap-levels (img:width first-image)
                               (img:height first-image)
                               depth)
      ;; TODO Move this to texture-map handling codes.
      #++(validate-mipmap-images
          images texture expected-mipmaps expected-resolutions)
      (loop :for (mipmap-width mipmap-height mipmap-depth)
              :in expected-resolutions
            :do (when (> (max mipmap-width mipmap-height mipmap-depth)
                         max-texture-3d-size)
                  (error "gpu-allocate-texture-storage[:texture-3d]: ~
                              Cannot load ~
                              texture-3d mipmap because one of its size ~
                              (width=~a, height=~a, depth=~a) is larger than ~
                              the maximum opengl 3d texture size: ~a"
                         mipmap-width mipmap-height mipmap-depth
                         max-texture-3d-size)))

      ;; Allocate storage for the texture.
      (let ((num-mipmaps-to-generate (if use-mipmaps-p
                                         (min expected-mipmaps max-mipmaps)
                                         1)))
        (if immutable-p
            ;; We can allocate the storage for the complete texture
            ;; all at once.
            (%gl:tex-storage-3d texture-type num-mipmaps-to-generate
                                (img:internal-format first-image)
                                (img:width first-image)
                                (img:height first-image)
                                depth)
            ;; We must allocate the complete texture layers individually.
            ;; TODO: We need to validate the internal-format and all that
            ;; are correct. The texture-map codes should do this.
            (loop :for i :below num-mipmaps-to-generate
                  :for (mipmap-width mipmap-height mipmap-depth)
                    :in expected-resolutions
                  :do (gl:tex-image-3d texture-type (+ texture-base-level i)
                                       (img:internal-format first-image)
                                       mipmap-width
                                       mipmap-height
                                       mipmap-depth
                                       0
                                       (img:pixel-format first-image)
                                       (img:pixel-type first-image)
                                       (cffi:null-pointer))))))))

(defmethod gpu-upload-texture-mipmap-layer ((texture-type (eql :texture-3d))
                                            texture image level
                                            &key offset face-signifier)
  (assert (integerp offset))
  (assert (null face-signifier))
  (gl:tex-sub-image-3d texture-type
                       level
                       0
                       0
                       offset
                       (img:width image)
                       (img:height image)
                       1
                       (img:pixel-format image)
                       (img:pixel-type image)
                       (img:data image)))
