(in-package #:colony.texture)

(defmethod load-texture-data ((texture-type (eql :texture-1d)) texture context)
  ;; TODO: This assumes no use of the general-data-descriptor or procedurally
  ;; generated content.
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level)))

    ;; load all of the images we may require.
    ;;
    ;; TODO: Reading the data here is an interim solution. Move to texture-map.
    (let ((images (read-texture-mipmaps texture :1d context)))

      ;; TODO: We have to read at least the headers of the images to get their
      ;; size and internal-format so we can allocate the right amount of space
      ;; on the GPU for the data of this texture.
      (gpu-allocate-texture-storage texture-type texture images)

      ;; Upload all of the mipmap image layers into the texture ram.
      ;; TODO: Allow scheduling of this work across frames.
      (loop :for idx :below (if use-mipmaps-p (length images) 1)
            :for level = (+ texture-base-level idx)
            :for image = (aref images idx)
            :do (gpu-upload-texture-mipmap-layer
                 texture-type texture image level))

      ;; Determine if opengl should generate the mipmaps.
      (potentially-autogenerate-mipmaps texture-type texture))))

(defmethod read-texture-mipmaps (texture (kind (eql :1d)) context)
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (data (get-computed-applied-attribute texture :data))
         (flip-y (get-computed-applied-attribute texture :flip-y)))

    ;; load all of the images we may require.
    ;;
    ;; TODO: Move the loading of the image data into main memory to texture-map
    ;; loading w/ the resource-cache, etc. This texture object should just have
    ;; a reference to the texture-map object holding the data.
    (let ((images (read-mipmap-images context data use-mipmaps-p :1d flip-y)))
      ;; Check to ensure they all fit into texture memory.
      ;; TODO: Refactor out of each method into validate-mipmap-images and
      ;; generalize.
      (loop :with max-size = c::=max-texture-size=
            :for image :across images
            :for location :across data
            :do (when (> (max (img:height image) (img:width image))
                         max-size)
                  (error
                   "Image ~a for 1D texture ~a is too big to be loaded onto ~
                    this card. Max resolution is ~a in either dimension."
                   location
                   (name texture)
                   max-size)))

      ;; The array of images which re the mipmaps.
      images)))

(defmethod gpu-allocate-texture-storage ((texture-type (eql :texture-1d))
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
           (- texture-max-level texture-base-level)))

    ;; TODO: Move the computing of the mipmap levels to the texture-map codes.
    (multiple-value-bind (expected-mipmaps expected-resolutions)
        (compute-mipmap-levels (img:width (aref images 0))
                               (img:height (aref images 0)))

      ;; TODO Move this to texture-map handling codes.
      (validate-mipmap-images
       images texture expected-mipmaps expected-resolutions)

      ;; Allocate storage for the texture.
      (let ((num-mipmaps-to-generate (if use-mipmaps-p
                                         (min expected-mipmaps max-mipmaps)
                                         1)))
        (if immutable-p
            ;; We can allocate the storage for the complete texture
            ;; all at once.
            (%gl:tex-storage-1d texture-type num-mipmaps-to-generate
                                (img:internal-format (aref images 0))
                                (img:width (aref images 0)))

            ;; We must allocate the complete texture layers individually.
            ;; TODO: We need to validate the internal-format and all that
            ;; are correct. The texture-map codes should do this.
            (loop :for idx :below (if use-mipmaps-p (length images) 1)
                  :for level = (+ texture-base-level idx)
                  :for image = (aref images idx)
                  :do (gl:tex-image-1d
                       texture-type
                       level
                       (img:internal-format image)
                       (img:width image)
                       0
                       (img:pixel-format image)
                       (img:pixel-type image)
                       (cffi:null-pointer))))))))

(defmethod gpu-upload-texture-mipmap-layer ((texture-type (eql :texture-1d))
                                            texture image level
                                            &key offset face-signifier)
  (assert (null offset))
  (assert (null face-signifier))
  (gl:tex-sub-image-1d texture-type
                       level
                       0
                       (img:width image)
                       (img:pixel-format image)
                       (img:pixel-type image)
                       (img:data image)))
