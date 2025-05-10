(in-package #:vutils)

(declaim (inline degrees->radians))
(defun degrees->radians (degrees)
  "Convert `DEGREES` to radians."
  (* degrees #.(/ pi 180)))

(declaim (inline radians->degrees))
(defun radians->degrees (radians)
  "Convert `RADIANS` to degrees."
  (* radians #.(/ 180 pi)))

(defun map-domain (source-min source-max dest-min dest-max value)
  "Map `VALUE` from the domain denoted by `SOURCE-MIN` and `SOURCE-MAX` to the
domain denoted by `DEST-MIN` and `DEST-MAX`."
  (alexandria:lerp (/ (- value source-min)
                      (- source-max source-min))
                   dest-min
                   dest-max))

(defun average (&rest numbers)
  "Calculate the mean average of `NUMBERS`, a list of numbers."
  (/ (reduce #'+ numbers) (length numbers)))

(defun compute-mipmap-levels (width height &optional (depth 1))
  "Compute how many mipmaps and what their resolutions must be given a WIDTH,
HEIGHT, and DEPTH (which defaults to 1) size of a texture. We follow Opengl's
formula in dealing with odd sizes (being rounded down). Return a values of: the
number of mipmap levels the list of resolutions from biggest to smallest each
mip map must have."
  (flet ((round-down (x)
           (ceiling (- x 1/2))))
    (let ((num-levels (1+ (floor (log (max width height depth) 2))))
          resolutions)
      (push (list width height depth) resolutions)
      (loop :with new-width = width
            :with new-height = height
            :with new-depth = depth
            :for level :below (1- num-levels)
            :do (setf new-width (max (round-down (/ new-width 2)) 1)
                      new-height (max (round-down (/ new-height 2)) 1)
                      new-depth (max (round-down (/ new-depth 2)) 1))
                (push (list new-width new-height new-depth) resolutions))
      (values num-levels (nreverse resolutions)))))
