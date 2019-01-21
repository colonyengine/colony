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
          :do (setf new-width (max (m:round-down (/ new-width 2)) 1)
                    new-height (max (m:round-down (/ new-height 2)) 1)
                    new-depth (max (m:round-down (/ new-depth 2)) 1))
              (push (list new-width new-height new-depth) resolutions))
    (values num-levels (nreverse resolutions))))

(defun reshape-image-array-layout (images-array)
  "Reshape an array in the form of:
 #(#(a0 a1 a2 ...) #(b0 b1 b2 ...) ...)
to
 #(#(a0 b0 ...) #(a1 b1 ...) #(a2 b2 ...) ...)
and return it."
  (let* ((w (length images-array))
         (h (length (aref images-array 0)))
         (reshaped (map-into (make-array h) (lambda () (make-array w)))))
    (dotimes (i w)
      (dotimes (j h)
        (setf (aref (aref reshaped j) i) (aref (aref images-array i) j))))
    reshaped))
