(in-package #:virality.examples)

;;; Textures

(v:define-texture art5/texture (:texture-2d)
  (:data #((:playground-tex "city.tiff"))))

;;; Materials

(v:define-material art1
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd:art1))

(v:define-material art2
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd:art2))

(v:define-material art3
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd:art3))

(v:define-material art4
  (:profiles (x/mat:u-mvptr)
   :uniforms ((:zoom 0.85f0)
              (:speed 1f0)
              (:strength 0.7f0)
              (:colorize nil)
              (:outline nil)
              (:detail 0.8f0))
   :shader ex/shd:art4))

(v:define-material art5
  (:profiles (x/mat:u-mvptr)
   :uniforms ((:blur 7.5f0)
              (:speed 0.24f0)
              (:zoom 0.75f0)
              (:sampler 'art5/texture))
   :shader shd/fx:window-rain))

(v:define-material art6
  (:profiles (x/mat:u-mvptr)
   :shader ex/shd:art6
   :uniforms ((:mouse (v2:vec)))))

;;; Components
(v:define-component mouse-shader-input ()
  ((%renderer :reader renderer)
   (%material :accessor material
              :initarg :material)
   (%material-retrieved-p :reader material-retrieved-p
                          :initform nil)
   (%mouse :reader mouse
           :initform (v2:vec))))

(defmethod v:on-component-initialize ((self mouse-shader-input))
  (with-slots (%renderer) self
    (setf %renderer (v:component-by-type (v:actor self) 'c/render:render))))


(defmethod v:on-component-update ((self mouse-shader-input))
  (with-slots (%material %material-retrieved-p) self
    (unless %material-retrieved-p
      (setf %material (c/render:material (renderer self))
            %material-retrieved-p t))

    (u:mvlet* ((context (v:context self))
               (x y (v:get-mouse-position context))
               (lmb-p (v:input-enabled-p context '(:mouse :left))))
      (when (null x) (setf x (/ (v:option context :window-width) 2.0f0)))
      (when (null y) (setf y (/ (v:option context :window-height) 2.0f0)))

      (when lmb-p
        (v2:with-components ((m (mouse self)))
          ;; crappy, but good enough.
          (setf mx (float (/ x (v:option context :window-width)) 1f0)
                my (float (/ y (v:option context :window-height)) 1f0)))
        (setf (v:uniform-ref %material :mouse) (mouse self))))))


;;; Prefabs

(v:define-prefab "art1" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2f0)
                                     (/ (v:option context :window-height) 2f0)
                                     0f0))
   (c/render:render :material 'art1)))

(v:define-prefab "art2" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2f0)
                                     (/ (v:option context :window-height) 2f0)
                                     0f0))
   (c/render:render :material 'art2)))

(v:define-prefab "art3" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2f0)
                                     (/ (v:option context :window-height) 2f0)
                                     0f0))
   (c/render:render :material 'art3)))

(v:define-prefab "art4" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2f0)
                                     (/ (v:option context :window-height) 2f0)
                                     0f0))
   (c/render:render :material 'art4)))

(v:define-prefab "art5" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2f0)
                                     (/ (v:option context :window-height) 2f0)
                                     0f0))
   (c/render:render :material 'art5)))

(v:define-prefab "art6" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (mouse-shader-input)
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2f0)
                                     (/ (v:option context :window-height) 2f0)
                                     0f0))
   (c/render:render :material 'art6)))

;;; Prefab descriptors

(v:define-prefab-descriptor art1 ()
  ("art1" examples))

(v:define-prefab-descriptor art2 ()
  ("art2" examples))

(v:define-prefab-descriptor art3 ()
  ("art3" examples))

(v:define-prefab-descriptor art4 ()
  ("art4" examples))

(v:define-prefab-descriptor art5 ()
  ("art5" examples))

(v:define-prefab-descriptor art6 ()
  ("art6" examples))
