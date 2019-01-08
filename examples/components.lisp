(in-package :first-light.example)

(fl:define-component shader-sweep ()
  ((renderer :default nil)
   (material :defualt nil)
   (material-retrieved-p :default nil)
   (mouse-in-window-p :default nil)
   (channel0 :default (flm:vec2))))

(defmethod fl:on-component-initialize ((self shader-sweep))
  (setf (renderer self) (fl:actor-component-by-type (fl:actor self) 'render)))

(defmethod fl:on-component-update ((self shader-sweep))
  (with-accessors ((renderer renderer) (material-copied-p material-copied-p)
                   (material material)
                   (material-retrieved-p material-retrieved-p)
                   (channel0 channel0) (max-x max-x) (max-y max-y))
      self
    (unless material-retrieved-p
      (setf material (fl.comp:material renderer)
            material-retrieved-p t))
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
      (setf (fl:mat-uniform-ref material :tex.channel0) channel0))))
