(in-package :fl.mfiano)

(define-component screen-drawing ()
  ((material :default nil)
   (vao-id :default nil)))

(defmethod initialize-component ((component screen-drawing) (context context))
  (with-slots (%material %vao-id) component
    (setf %material (lookup-material %material context)
          %vao-id (gl:gen-vertex-array))))

(defmethod render-component ((component screen-drawing) (context context))
  (with-slots (%material %vao-id) component
    (shadow:with-shader-program (shader %material)
      (bind-material %material)
      (gl:bind-vertex-array %vao-id)
      (gl:draw-arrays :points 0 1))))
