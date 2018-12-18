(in-package :%first-light)

(defmethod load-texture-data ((texture-type (eql :texture-cube-map-array)) texture context)
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data)))

    ;; load all of the images we may require.
    (let* ((images (read-mipmap-images context data use-mipmaps-p
                                       :cube-map-array))
           (first-cube (aref images 0))
           (first-image (aref (second (aref first-cube 0)) 0))
           ;; TODO: This is not safe, need to check all of them.
           ;; TODO: WEe assume all cube maps have the same mipmap number,
           ;; I don't know if this is a reuirements or not.
           (num-mipmaps (length (second (aref first-cube 0)))))

      (v:debug :fl.core.texture
               "Loading :texture-cube-map-array images = ~a"
               images)

      ;; Figure out the ideal mipmap count from the base resolution.
      (multiple-value-bind (expected-mipmaps expected-resolutions)
          ;; TODO: This might need work with cube-maps.
          (compute-mipmap-levels (width first-image)
                                 (height first-image))
        (declare (ignore expected-resolutions))


        ;; TODO: Fix this up for cube-maps
        #++(validate-mipmap-images images texture
                                   expected-mipmaps expected-resolutions)

        (potentially-degrade-texture-min-filter texture)

        ;; Allocate immutable storage if required.
        (when immutable-p
          (let ((num-mipmaps-to-generate
                  (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
            (v:debug :fl.core.texture "tex-storage-3d: texture-type = ~A, num-mipmaps-to-generate = ~A, internal-format = ~A, width = ~A, height = ~A, depth = ~A~%"
                     texture-type num-mipmaps-to-generate
                     (internal-format first-image)
                     (width first-image)
                     (height first-image)
                     (* (length data) 6))
            (%gl:tex-storage-3d texture-type num-mipmaps-to-generate
                                (internal-format first-image)
                                (width first-image)
                                (height first-image)
                                ;; For this target, we need number of
                                ;; layer-face, not number of layer like in
                                ;; cube-maps!
                                (* (length data) 6))))

        ;; Insert all cubes plus mipmaps into the GPU.
        (loop
          :for cube :across images
          :for cube-idx :below (length images)
          :do
             (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
                   :for level = (+ texture-base-level idx)
                   ;; Now load all six faces, each face at the same mipmap level.
                   :do (loop :for (face-signifier mipmaps) :across cube
                             ;; NOTE: face-idx works cause I sorted the faces
                             ;; earlier.
                             :for face-idx :by 1
                             :do (v:debug :fl.core.texture "inserting cube ~A face ~A[~A]~%"
                                          cube-idx face-signifier idx)
                                 (with-slots (%width %height %internal-format
                                              %pixel-format %pixel-type %data)
                                     (aref mipmaps idx)
                                   (if immutable-p
                                       (gl:tex-sub-image-3d texture-type level
                                                            0 0
                                                            (+ (* cube-idx 6) face-idx)
                                                            %width %height 1
                                                            %pixel-format
                                                            %pixel-type %data)

                                       (gl:tex-image-3d texture-type level
                                                        %internal-format
                                                        %width %height
                                                        (+ (* cube-idx 6) face-idx)
                                                        0
                                                        %pixel-format %pixel-type
                                                        %data))))))
        (free-mipmap-images images :cube-map-array)
        (potentially-autogenerate-mipmaps texture-type texture)))))
