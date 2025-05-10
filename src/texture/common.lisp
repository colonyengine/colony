(in-package #:colony.texture)

(u:define-constant +sampler-type->texture-type+
    (u:dict :sampler-1d :texture-1d
            :isampler-1d :texture-1d
            :usampler-1d :texture-1d
            :sampler-2d :texture-2d
            :isampler-2d :texture-2d
            :usampler-2d :texture-2d
            :sampler-3d :texture-3d
            :isampler-3d :texture-3d
            :usampler-3d :texture-3d
            :sampler-cube :texture-cube-map
            :isampler-cube :texture-cube-map
            :usampler-cube :texture-cube-map
            :sampler-2d-rect :texture-rectangle
            :isampler-2d-rect :texture-rectangle
            :usampler-2d-rect :texture-rectangle
            :sampler-1d-array :texture-1d-array
            :isampler-1d-array :texture-1d-array
            :usampler-1d-array :texture-1d-array
            :sampler-2d-array :texture-2d-array
            :isampler-2d-array :texture-2d-array
            :usampler-2d-array :texture-2d-array
            :sampler-cube-array :texture-cube-map-array
            :isampler-cube-array :texture-cube-map-array
            :usampler-cube-array :texture-cube-map-array
            :sampler-buffer :texture-buffer
            :isampler-buffer :texture-buffer
            :usampler-buffer :texture-buffer
            :sampler-2d-ms :texture-2d-multisample
            :isampler-2d-ms :texture-2d-multisample
            :usampler-2d-ms :texture-2d-multisample
            :sampler-2d-ms-array :texture-2d-multisample-array
            :isampler-2d-ms-array :texture-2d-multisample-array
            :usampler-2d-ms-array :texture-2d-multisample-array)
  :test #'equalp
  :documentation
  "This variable is a hash table to map sampler types to texture types. It is a
constant and will never be changed at runtime.")

(u:define-constant +cube-map-face->texture-type+
    (u:dict :+x '(:texture-cube-map-positive-x 0)
            :-x '(:texture-cube-map-negative-x 1)
            :+y '(:texture-cube-map-positive-y 2)
            :-y '(:texture-cube-map-negative-y 3)
            :+z '(:texture-cube-map-positive-z 4)
            :-z '(:texture-cube-map-negative-z 5)
            ;; WYSIWYG shortcut name to real layer name
            :right '(:texture-cube-map-positive-x 0)
            :left '(:texture-cube-map-negative-x 1)
            :top '(:texture-cube-map-positive-y 2)
            :bottom '(:texture-cube-map-negative-y 3)
            :back '(:texture-cube-map-positive-z 4)
            :front '(:texture-cube-map-negative-z 5))
  :test #'equalp
  :documentation
  "This variable converts between an easy human (semantic) name for a cube map
 face to the complicated opengl name.")

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

(defun canonicalize-cube-map-face-signfier (face-signifier)
  (first (u:href +cube-map-face->texture-type+ face-signifier)))

(defun sort-cube-map-faces-func (left right &key (test #'<))
  (let ((l (u:href +cube-map-face->texture-type+ left))
        (r (u:href +cube-map-face->texture-type+ right)))
    (funcall test (second l) (second r))))
