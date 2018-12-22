(in-package :%first-light)

(defmethod load-texture-data ((texture-type (eql :texture-3d)) texture context)
  ;; Determine if loading :images or :volume

  ;; TODO: Validating a 3d texture.
  ;; 0. Ensure that each mipmap level has the same number of slices.
  ;; 1. Ensure that all mipmap levels have the same number of slices.
  ;; 2. Ensure that all required mipmaps levels are present and correct.
  ;; 2. Ensure that each mipmap level fits into the current limits on the card.

  ;; TODO: Fix me. Need to support :layout properly. Currently, this form
  ;; represents opengl defaults.
  (let ((hardcoded-layout '((:origin :left-back-bottom)
                            (:shape (:slices :back-to-front))))
        (current-layout (get-computed-applied-attribute texture :layout)))
    (unless (equal current-layout hardcoded-layout)
      (error "3D Texture ~A has layout:~%  ~S~%but it can only have this as its layout:~%  ~S"
             (name texture) current-layout hardcoded-layout)))

  (let* ((use-mipmaps-p
           (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p
           (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (max-texture-3d-size (gl:get-integer :max-3d-texture-size))
         (data (get-computed-applied-attribute texture :data))
         (num-mipmaps (length data)))

    #++(format t "Attempting to load 3d texture ~A onto GPU: immutable = ~A~%"
               (name texture) immutable-p)

    ;; Load all of our images for each mipmap level, if needed.
    (let* ((all-slices (read-mipmap-images context data use-mipmaps-p :3d))
           (first-image (aref (aref all-slices 0) 0))
           (depth (length (aref all-slices 0))))

      ;; Figure out the ideal mipmap count from the base resolution.
      (multiple-value-bind (expected-mipmaps expected-resolutions)
          (compute-mipmap-levels (fl.image:width first-image)
                                 (fl.image:height first-image)
                                 depth)
        #++(format t "expected mipmaps: ~A expected-resolutions: ~A~%"
                   expected-mipmaps expected-resolutions)

        ;; TODO: Fix this call.
        #++(validate-mipmap-images images texture
                                   expected-mipmaps expected-resolutions)
        (potentially-degrade-texture-min-filter texture)

        ;; Allocate immutable storage if required.
        (let ((num-mipmaps-to-generate
                (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
          (if immutable-p
              (%gl:tex-storage-3d texture-type num-mipmaps-to-generate
                                  (fl.image:internal-format first-image)
                                  (fl.image:width first-image)
                                  (fl.image:height first-image)
                                  depth)
              (dotimes (i num-mipmaps-to-generate)
                (gl:tex-image-3d texture-type i
                                 (fl.image:internal-format first-image)
                                 (fl.image:width first-image)
                                 (fl.image:height first-image)
                                 (length (aref all-slices i)) 0
                                 (fl.image:pixel-format first-image)
                                 (fl.image:pixel-type first-image)
                                 (cffi:null-pointer)))))

        ;; Load all volumetric mipmaps into the gpu.
        (loop :for idx :below (if use-mipmaps-p num-mipmaps 1)
              :for level = (+ texture-base-level idx)
              :for (mipmap-width mipmap-height mipmap-depth)
                :in expected-resolutions
              :do (with-accessors ((width fl.image:width)
                                   (height fl.image:height)
                                   (internal-format fl.image:internal-format)
                                   (pixel-format fl.image:pixel-format)
                                   (pixel-type fl.image:pixel-type))
                      (aref (aref all-slices idx) 0)
                    #++(format t "Uploading tp GPU 3d mipmap image at level ~A with resolution (w:~A h:~A d:~A)~%"
                               level mipmap-width mipmap-height mipmap-depth)

                    ;; TODO: I should move this error check to the validation
                    ;; stage above instead of being here.
                    (when (> (max mipmap-width mipmap-height mipmap-depth)
                             max-texture-3d-size)
                      (error "load-texture-data[:texture-3d]: Cannot load texture-3d mipmap because one of its size (width=~A, height=~A, depth=~A) is larger than the maximum opengl 3d texture size: ~A"
                             mipmap-width mipmap-height mipmap-depth
                             max-texture-3d-size))
                    (dotimes (z (length (aref all-slices idx)))
                      (gl:tex-sub-image-3d texture-type level 0 0 z
                                           width height 1
                                           pixel-format pixel-type
                                           (fl.image:data
                                            (aref (aref all-slices idx) z))))))

        (free-mipmap-images all-slices :3d)

        (potentially-autogenerate-mipmaps texture-type texture)

        ))))
