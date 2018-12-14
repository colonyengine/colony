(in-package #:first-light.example)

;; This component reads the mouse position and puts it into a uniform in the
;; material in the mesh-renderer associated with the actor this component
;; is on. The material for that mesh-renderer knows to use extra uniforms to
;; pass that information to the shader so as you move the mouse around the
;; shader uses the position as noise input to manipulate the texture in
;; realtime.


(define-component shader-sweep ()
  ((mesh-rend :default nil)
   (mesh-rend-material :defualt nil)
   (mesh-material-retrieved-p :default nil)
   (mouse-in-window-p :default nil)
   (channel0 :default (flm:vec2))))


(defmethod initialize-component ((self shader-sweep) (context context))
  ;; Find the mesh-renderer on my actor.
  (setf (mesh-rend self)
        (actor-component-by-type (actor self) 'mesh-renderer)))


(defmethod update-component ((self shader-sweep) (context context))
  (with-accessors ((mesh-rend mesh-rend) (material-copied-p material-copied-p)
                   (mesh-rend-material mesh-rend-material)
                   (mesh-material-retrieved-p mesh-material-retrieved-p)
                   (channel0 channel0) (max-x max-x) (max-y max-y))
      self

    (unless mesh-material-retrieved-p
      (setf mesh-rend-material (material mesh-rend)
            mesh-material-retrieved-p t))

    (fl.util:mvlet* ((x y (fl.input:get-mouse-position (input-data context))))
      (when (null x) (setf x (/ (option context :window-width) 2.0)))
      (when (null y) (setf y (/ (option context :window-height) 2.0)))
      (flm:with-vec2 ((c channel0))
        ;; crappy, but good enough.
        (setf c.x (coerce (/ x (option context :window-width)) 'single-float)
              c.y (coerce (/ y (option context :window-height)) 'single-float)))

      (when (fl.input:input-enter-p (input-data context) '(:mouse :left))
        (format t "channel0 is: ~A~%" channel0))

      ;; get a reference to the material itself (TODO: use MOP stuff to get
      ;; this right so I don't always have to get it here)
      (setf (mat-uniform-ref mesh-rend-material :tex.channel0) channel0))))
