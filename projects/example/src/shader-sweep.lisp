(in-package #:fl.example)

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
   (channel0 :default (v2:zero))))


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

    (fu:mvlet* ((x y (get-mouse-position context)))
      (when (null x) (setf x (/ (cfg context :window-width) 2.0)))
      (when (null y) (setf y (/ (cfg context :window-height) 2.0)))
      (v2:with-components ((c channel0))
        ;; crappy, but good enough.
        (setf cx (coerce (/ x (cfg context :window-width)) 'single-float)
              cy (coerce (/ y (cfg context :window-height)) 'single-float)))

      (when (input-enter-p context '(:mouse :left))
        (format t "channel0 is: ~A~%" channel0))

      ;; get a reference to the material itself (TODO: use MOP stuff to get
      ;; this right so I don't always have to get it here)
      (setf (mat-uniform-ref mesh-rend-material :tex.channel0) channel0))))
