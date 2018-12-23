(in-package #:first-light.example)

;; This component reads the mouse position and puts it into a uniform in the
;; material in the mesh-renderer associated with the actor this component
;; is on. The material for that mesh-renderer knows to use extra uniforms to
;; pass that information to the shader so as you move the mouse around the
;; shader uses the position as noise input to manipulate the texture in
;; realtime.


(fl:define-component shader-sweep ()
  ((mesh-rend :default nil)
   (mesh-rend-material :defualt nil)
   (mesh-material-retrieved-p :default nil)
   (mouse-in-window-p :default nil)
   (channel0 :default (flm:vec2))))


(defmethod fl:on-component-initialize ((self shader-sweep))
  ;; Find the mesh-renderer on my actor.
  (setf (mesh-rend self) (fl:actor-component-by-type (fl:actor self) 'mesh-renderer)))

(defmethod fl:on-component-update ((self shader-sweep))
  (with-accessors ((mesh-rend mesh-rend) (material-copied-p material-copied-p)
                   (mesh-rend-material mesh-rend-material)
                   (mesh-material-retrieved-p mesh-material-retrieved-p)
                   (channel0 channel0) (max-x max-x) (max-y max-y))
      self
    (unless mesh-material-retrieved-p
      (setf mesh-rend-material (fl.comp:material mesh-rend)
            mesh-material-retrieved-p t))
    (fl.util:mvlet* ((context (fl:context self))
                     (x y (fl.input:get-mouse-position (fl:input-data context))))
      (when (null x) (setf x (/ (fl:option context :window-width) 2.0)))
      (when (null y) (setf y (/ (fl:option context :window-height) 2.0)))
      (flm:with-vec2 ((c channel0))
        ;; crappy, but good enough.
        (setf c.x (float (/ x (fl:option context :window-width)) 1f0)
              c.y (float (/ y (fl:option context :window-height)) 1f0)))
      ;; get a reference to the material itself (TODO: use MOP stuff to get
      ;; this right so I don't always have to get it here)
      (setf (fl:mat-uniform-ref mesh-rend-material :tex.channel0) channel0))))
