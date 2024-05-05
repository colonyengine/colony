(in-package #:colony.texture)

(defmethod load-texture-data ((texture-type (eql :texture-cube-map))
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
    (let* ((images (read-texture-mipmaps texture :cube-map context))
           (first-cube (aref images 0))
           (num-mipmaps (length (second (aref (second first-cube) 0)))))

      ;; TODO: We have to read at least the headers of the images to get their
      ;; size and internal-format so we can allocate the right amount of space
      ;; on the GPU for the data of this texture.
      (gpu-allocate-texture-storage texture-type texture images)

      ;; Upload all of the mipmap image layers into the texture ram.
      ;; TODO: Allow scheduling of this work across frames.
      (loop :for (layout cube) :across images ;; only 1 cube available.
            :do (dotimes (idx (if use-mipmaps-p num-mipmaps 1))
                  (loop :with level = (+ texture-base-level idx)
                        :for (face-signifier mipmaps) :across cube
                        :do (assert (equal layout '(:layout :opengl)))
                            (let ((image (aref mipmaps idx)))
                              (gpu-upload-texture-mipmap-layer
                               texture-type
                               texture
                               image
                               level
                               :face-signifier face-signifier)))))


      ;; Determine if opengl should generate the mipmaps.
      (potentially-autogenerate-mipmaps texture-type texture))))


(defmethod read-texture-mipmaps (texture (kind (eql :cube-map)) context)
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (flip-y (get-computed-applied-attribute texture :flip-y))
         (data (get-computed-applied-attribute texture :data)))

    ;; load all of the images we may require.
    ;;
    ;; TODO: Move the loading of the image data into main memory to texture-map
    ;; loading w/ the resource-cache, etc. This texture object should just have
    ;; a reference to the texture-map object holding the data.
    (let ((images (read-mipmap-images
                   context data use-mipmaps-p :cube-map flip-y)))
      ;; TODO: Check to ensure they all fit into texture memory.

      ;; The reshaped array of images which are the mipmaps.
      images)))

(defmethod gpu-allocate-texture-storage ((texture-type (eql :texture-cube-map))
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
         (first-cube (aref images 0))
         (first-image (aref (second (aref (second first-cube) 0)) 0))
         (num-mipmaps (length (second (aref (second first-cube) 0)))))

    ;; TODO: Due to the vagaries of the current texture codes, we must ensure
    ;; that the user made no error in specifying the cube map.
    (assert (= (length images) 1))

    ;; Check to ensure they all fit into texture memory.
    ;; TODO: Refactor out of each method into validate-mipmap-images and
    ;; generalize.
    (loop :with max-size = c::=max-texture-size=
          :for (placement mipmaps) :across (second first-cube)
          :for image = (aref mipmaps 0)
          :do
             (when (> (max (img:height image) (img:width image))
                      max-size)
               ;; TODO: print out the location of the failing image.
               (error "An Image for texture ~a is too big to be loaded onto ~
                        this card. Max resolution is ~a in either dimension."
                      (name texture)
                      max-size)))

    ;; TODO: Move the computing of the mipmap levels to the texture-map codes.
    (multiple-value-bind (expected-mipmaps expected-resolutions)
        (compute-mipmap-levels (img:width first-image)
                               (img:height first-image))
      (declare (ignore expected-resolutions))

      ;; TODO Move this to texture-map handling codes. Fix for cube-maps.
      #++(validate-mipmap-images
          images texture expected-mipmaps expected-resolutions)

      ;; Allocate storage for the texture.
      (let ((num-mipmaps-to-generate (if use-mipmaps-p
                                         (min expected-mipmaps max-mipmaps)
                                         1)))
        (if immutable-p
            ;; We can allocate the storage for the complete texture
            ;; all at once.
            (%gl:tex-storage-2d texture-type num-mipmaps-to-generate
                                (img:internal-format first-image)
                                (img:width first-image)
                                (img:height first-image))

            ;; TODO: Look in red book for how to allocate the storage
            ;; required for a cube map using this interface.
            ;;
            ;; We must allocate the complete texture layers individually
            ;; for each face.
            ;; TODO: We need to validate the internal-format and all that
            ;; are correct. The texture-map codes should do this.
            ;;
            ;; TODO: This is pretty crappy, rewrite to only do this with the
            ;; knowledge we have a single cube to deal with.
            (loop :for (layout cube) :across images ;; only 1 cube available.
                  :do (dotimes (idx (if use-mipmaps-p num-mipmaps 1))
                        (loop :with level = (+ texture-base-level idx)
                              :for (face-signifier mipmaps) :across cube
                              :do (assert (equal layout '(:layout :opengl)))
                                  (let ((image (aref mipmaps idx)))
                                    (gl:tex-image-2d
                                     face-signifier
                                     level
                                     (img:internal-format image)
                                     (img:width image)
                                     (img:height image)
                                     0
                                     (img:pixel-format image)
                                     (img:pixel-type image)
                                     (img:data image)))))))))))


(defmethod gpu-upload-texture-mipmap-layer ((texture-type
                                             (eql :texture-cube-map))
                                            texture image level
                                            &key offset face-signifier)
  (assert (null offset))
  (assert (and face-signifier (symbolp face-signifier)))
  (gl:tex-sub-image-2d face-signifier
                       level
                       0
                       0
                       (img:width image)
                       (img:height image)
                       (img:pixel-format image)
                       (img:pixel-type image)
                       (img:data image)))
