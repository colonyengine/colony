(in-package #:colony-examples)

;; TODO: Remove uses of globals and replace with calls to cfg system. This is
;; because Colony should not provide any globals to the user, and also because
;; the screen size can change.

;;; Textures

(c:define-texture-map art5/texture (:2d :unique)
  (texmap:mipmap (textures city)))

(c:define-texture art5/texture (:texture-2d)
  ;; TODO: TMAP Convert :data to use texture-map name.
  (:data #((textures city))))

;;; Materials

(c:define-material art1
  (:profiles (x:u-mvptr)
   :shader ex/shd:art1))

(c:define-material art2
  (:profiles (x:u-mvptr)
   :shader ex/shd:art2))

(c:define-material art3
  (:profiles (x:u-mvptr)
   :shader ex/shd:art3))

(c:define-material art4
  (:profiles (x:u-mvptr)
   :uniforms ((:zoom 0.85f0)
              (:speed 1f0)
              (:strength 0.7f0)
              (:colorize nil)
              (:outline nil)
              (:detail 0.8f0))
   :shader ex/shd:art4))

(c:define-material art5
  (:profiles (x:u-mvptr)
   :uniforms ((:blur 7.5f0)
              (:speed 0.24f0)
              (:zoom 0.75f0)
              (:sampler 'art5/texture))
   :shader ex/shd:art5))

(c:define-material art6
  (:profiles (x:u-mvptr)
   :shader ex/shd:art6
   :uniforms ((:mouse (v2:zero)))))

;;; Components

(c:define-component mouse-shader-input ()
  ((%renderer :reader renderer)
   (%material :accessor material
              :initarg :material)
   (%material-retrieved-p :reader material-retrieved-p
                          :initform nil)
   (%mouse :reader mouse
           :initform (v2:zero))))

(defmethod c:on-component-initialize ((self mouse-shader-input))
  (with-slots (%renderer) self
    (setf %renderer (c:component-by-type (c:actor self) 'comp:render))))

(defmethod c:on-component-update ((self mouse-shader-input))
  (with-slots (%material %material-retrieved-p) self
    (unless %material-retrieved-p
      (setf %material (comp:material (renderer self))
            %material-retrieved-p t))
    (u:mvlet* ((context (c:context self))
               (x y (c:get-mouse-position context))
               (lmb-p (c:on-button-enabled context :mouse :left)))
      (when (null x) (setf x (/ c:=window-width= 2f0)))
      (when (null y) (setf y (/ c:=window-height= 2f0)))
      (when lmb-p
        (v2:with-components ((m (mouse self)))
          (setf mx (float (/ x c:=window-width=) 1f0)
                my (float (/ y c:=window-height=) 1f0)))
        (setf (c:uniform-ref %material :mouse) (mouse self))))))

;;; Prefabs

(c:define-prefab "art1" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ c:=window-width= 2f0)
                                  (/ c:=window-height= 2f0)
                                  0f0))
   (comp:render :material 'art1
                :slave (c:ref :self :component 'comp:mesh))))

(c:define-prefab "art2" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ c:=window-width= 2f0)
                                  (/ c:=window-height= 2f0)
                                  0f0))
   (comp:render :material 'art2
                :slave (c:ref :self :component 'comp:mesh))))

(c:define-prefab "art3" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ c:=window-width= 2f0)
                                  (/ c:=window-height= 2f0)
                                  0f0))
   (comp:render :material 'art3
                :slave (c:ref :self :component 'comp:mesh))))

(c:define-prefab "art4" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ c:=window-width= 2f0)
                                  (/ c:=window-height= 2f0)
                                  0f0))
   (comp:render :material 'art4
                :slave (c:ref :self :component 'comp:mesh))))

(c:define-prefab "art5" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ c:=window-width= 2f0)
                                  (/ c:=window-height= 2f0)
                                  0f0))
   (comp:render :material 'art5
                :slave (c:ref :self :component 'comp:mesh))))

(c:define-prefab "art6" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("screen" :copy "/mesh")
   (mouse-shader-input)
   (comp:transform :scale (v3:vec (/ c:=window-width= 2f0)
                                  (/ c:=window-height= 2f0)
                                  0f0))
   (comp:render :material 'art6
                :slave (c:ref :self :component 'comp:mesh))))
