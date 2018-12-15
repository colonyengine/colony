(in-package :%first-light)

(defun compute-mipmap-levels (width height &optional (depth 1))
  "Compute how many mipmaps and what their resolutions must be given a
WIDTH, HEIGHT, and DEPTH (which defaults to 1) size of a texture. We
follow Opengl's formula in dealing with odd sizes (being rounded down).
Return a values of:
  the number of mipmap levels
  the list of resolutions from biggest to smallest each mip map must have."
  (let ((num-levels (1+ (floor (log (max width height depth) 2))))
        (resolutions nil))
    (push (list width height depth) resolutions)
    (loop :with new-width = width
          :with new-height = height
          :with new-depth = depth
          :for level :below (1- num-levels)
          :do (setf new-width (max (flm:round-down (/ new-width 2)) 1)
                    new-height (max (flm:round-down (/ new-height 2)) 1)
                    new-depth (max (flm:round-down (/ new-depth 2)) 1))
              (push (list new-width new-height new-depth) resolutions))
    (values num-levels (nreverse resolutions))))


(defun slices-to-volume (images)
  "Take an array of image objects in IMAGES, all the same height and width and
mipmap level, and assuming index 0 is the 'back slice' and the highest index is
the 'front slice' (according to opengl's 3d texture definition) and then
assemble them into a single linear array suitable for a 3d texture for
opengl. Return a linear array of UNSIGNED-BYTEs that hold the volumentric data."
  (let* ((first-slice (aref images 0))
         (max-depth (length images))
         (pixel-size (get-pixel-size first-slice)))

    (with-slots (%width %height) first-slice ;; all slices the same.
      (let* ((volume-data-size (* %width %height max-depth pixel-size))
             (volume-data (make-array volume-data-size
                                      :element-type '(unsigned-byte 8))))

        #++(format t "slices-to-volume: slices = ~A, volume-data-size = ~A bytes~%"
                   (length images) volume-data-size)

        ;; Recontextualize the 3d images into a real volume array specified as
        ;; a linear array.
        ;;
        ;; NOTE: Assuming row major ordering for all arrays.
        (loop :for d :below max-depth
              :for image = (aref images d) :do
              #++(format t "Processing slice image index ~A: Image ~A~%"
                         d image)
              ;; x and y in terms of images.
                 (loop :for w :below %width :do
                   (loop :for h :below %height :do
                     (let* ((pixel-start-2d
                              (+ (* h (* %width pixel-size))
                                 (* w pixel-size)))
                            (pixel-start-3d
                              (+ (* d (* %width %height pixel-size))
                                 (* h (* %width pixel-size))
                                 (* w pixel-size))))

                       ;; TODO: Crappily copy over the individual pixel data
                       ;; from the 2d image to the 3d image. I should poke this
                       ;; more to see if I can copy way more data at once.
                       (replace volume-data (data image)
                                :start1 pixel-start-3d
                                :end1 (+ pixel-start-3d pixel-size)
                                :start2 pixel-start-2d
                                :end2 (+ pixel-start-2d pixel-size))))))

        #++(format t "Finished constructing volume data.~%")
        volume-data))))

(defun lines-to-plane (images)
  "Take an array of image objects in IMAGES, all the same width and mipmap
level, and assuming index 0 is the first layer and the highest index is the last
layer (according to opengl's 1d texture array texture definition) and then
assemble them into a single linear array suitable for a 2d texture for
opengl. Return a linear array of UNSIGNED-BYTEs that hold the planar data."
  (let* ((first-slice (aref images 0))
         (max-height (length images))
         (pixel-size (get-pixel-size first-slice)))

    (with-slots (%width) first-slice ;; all slices the same.
      (let* ((planar-data-size (* %width max-height pixel-size))
             (planar-data (make-array planar-data-size
                                      :element-type '(unsigned-byte 8))))

        ;; Recontextualize the 1d images into a planar array specified as
        ;; a linear array.
        ;;
        ;; NOTE: Assuming row major ordering for all arrays.
        (loop :for h :below max-height
              :for image = (aref images h) :do
                (let* ((row-start-2d
                         ;; always start at start of a 2d row.
                         (+ (* h (* %width pixel-size))
                            (* 0 pixel-size)))
                       ;; Always starting at start of 1d row.
                       (row-start-1d 0))

                  ;; Copy the whole 1d line into the 2d image.
                  (replace planar-data (data image)
                           :start1 row-start-2d
                           :end1 (+ row-start-2d
                                    (* %width pixel-size))
                           :start2 row-start-1d
                           :end2 (+ row-start-1d
                                    (* %width pixel-size)))))
        planar-data))))


(defun reshape-image-array-layout (images-array)
  "Reshape and array in the form of:
 #(#(a0 a1 a2 ...) #(b0 b1 b2 ...) ...)
to
 #(#(a0 b0 ...) #(a1 b1 ...) #(a2 b2 ...) ...)
and return it."
  (apply #'map 'vector (lambda (&rest elems)
                         (coerce elems 'vector))
         (coerce images-array 'list)))
