(in-package #:colony-examples)

;;; Texture Maps

(c:define-texture-map helmet-albedo (:single :unique)
  (:mipmap () (mesh-textures helmet-albedo)))

(c:define-texture-map helmet-ao (:single :unique)
  (:mipmap () (mesh-textures helmet-ao)))

(c:define-texture-map helmet-emissive (:single :unique)
  (:mipmap () (mesh-textures helmet-emissive)))

(c:define-texture-map helmet-metallic-roughness (:single :unique)
  (:mipmap () (mesh-textures helmet-metallic-roughness)))

(c:define-texture-map helmet-normal (:single :unique)
  (:mipmap () (mesh-textures helmet-normal)))

;; TODO: These next cube map entries are for GLTF physically based
;; rendering. We probably should write a macro to help generate these forms.

;; Doge2 cube maps
(c:define-texture-map doge2-diffuse-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-diffuse-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-diffuse-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-diffuse-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-diffuse-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-diffuse-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-diffuse-front)))))

(c:define-texture-map doge2-specular-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-specular-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-specular-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-specular-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-specular-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-specular-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments doge2-specular-front)))))

;; Papermill cube maps
(c:define-texture-map papermill-diffuse-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-diffuse-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-diffuse-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-diffuse-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-diffuse-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-diffuse-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-diffuse-front)))))

(c:define-texture-map papermill-specular-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-specular-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-specular-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-specular-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-specular-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-specular-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments papermill-specular-front)))))

;; Helipad cube maps
(c:define-texture-map helipad-diffuse-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-diffuse-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-diffuse-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-diffuse-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-diffuse-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-diffuse-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-diffuse-front)))))

(c:define-texture-map helipad-specular-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-specular-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-specular-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-specular-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-specular-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-specular-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments helipad-specular-front)))))

;; pisa cube maps
(c:define-texture-map pisa-diffuse-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-diffuse-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-diffuse-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-diffuse-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-diffuse-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-diffuse-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-diffuse-front)))))

(c:define-texture-map pisa-specular-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-specular-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-specular-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-specular-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-specular-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-specular-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments pisa-specular-front)))))

;; Footprint cube maps
(c:define-texture-map footprint-diffuse-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-diffuse-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-diffuse-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-diffuse-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-diffuse-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-diffuse-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-diffuse-front)))))

(c:define-texture-map footprint-specular-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-specular-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-specular-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-specular-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-specular-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-specular-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments footprint-specular-front)))))

;; ennis cube maps
(c:define-texture-map ennis-diffuse-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-diffuse-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-diffuse-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-diffuse-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-diffuse-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-diffuse-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-diffuse-front)))))

(c:define-texture-map ennis-specular-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-specular-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-specular-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-specular-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-specular-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-specular-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments ennis-specular-front)))))

;; field cube maps
(c:define-texture-map field-diffuse-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-diffuse-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-diffuse-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-diffuse-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-diffuse-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-diffuse-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-diffuse-front)))))

(c:define-texture-map field-specular-cube (:cube :unique :six)
  (:face ((:dir :+x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-specular-right))))
  (:face ((:dir :-x))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-specular-left))))
  (:face ((:dir :+y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-specular-top))))
  (:face ((:dir :-y))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-specular-bottom))))
  (:face ((:dir :+z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-specular-back))))
  (:face ((:dir :-z))
         (c:define-texture-map nil (:single :unique)
           (:mipmap () (environments field-specular-front)))))

;; BRDF-LUT for physically based rendering.
(c:define-texture-map brdf-lut (:single :unique)
  (:mipmap () (c:textures c::brdf-lut)))

;;; Textures

(c:define-texture damaged-helmet/mesh (:texture-2d-array)
  (:flip-y t)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name.
   #(#((mesh-textures helmet-albedo))
     #((mesh-textures helmet-ao))
     #((mesh-textures helmet-emissive))
     #((mesh-textures helmet-metallic-roughness))
     #((mesh-textures helmet-normal)))))

(c:define-texture doge2 (:texture-cube-map-array)
  (:texture-min-filter :linear-mipmap-linear)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name.
   #(((:layout :six)
      #((:+x #((environments doge2-diffuse-right)))
        (:-x #((environments doge2-diffuse-left)))
        (:+y #((environments doge2-diffuse-top)))
        (:-y #((environments doge2-diffuse-bottom)))
        (:+z #((environments doge2-diffuse-front)))
        (:-z #((environments doge2-diffuse-back)))))
     ((:layout :six)
      #((:+x #((environments doge2-specular-right)))
        (:-x #((environments doge2-specular-left)))
        (:+y #((environments doge2-specular-top)))
        (:-y #((environments doge2-specular-bottom)))
        (:+z #((environments doge2-specular-front)))
        (:-z #((environments doge2-specular-back))))))))

(c:define-texture papermill (:texture-cube-map-array)
  (:texture-min-filter :linear-mipmap-linear)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name.
   #(((:layout :six)
      #((:+x #((environments papermill-diffuse-right)))
        (:-x #((environments papermill-diffuse-left)))
        (:+y #((environments papermill-diffuse-top)))
        (:-y #((environments papermill-diffuse-bottom)))
        (:+z #((environments papermill-diffuse-front)))
        (:-z #((environments papermill-diffuse-back)))))
     ((:layout :six)
      #((:+x #((environments papermill-specular-right)))
        (:-x #((environments papermill-specular-left)))
        (:+y #((environments papermill-specular-top)))
        (:-y #((environments papermill-specular-bottom)))
        (:+z #((environments papermill-specular-front)))
        (:-z #((environments papermill-specular-back))))))))

(c:define-texture helipad (:texture-cube-map-array)
  (:texture-min-filter :linear-mipmap-linear)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name.
   #(((:layout :six)
      #((:+x #((environments helipad-diffuse-right)))
        (:-x #((environments helipad-diffuse-left)))
        (:+y #((environments helipad-diffuse-top)))
        (:-y #((environments helipad-diffuse-bottom)))
        (:+z #((environments helipad-diffuse-front)))
        (:-z #((environments helipad-diffuse-back)))))
     ((:layout :six)
      #((:+x #((environments helipad-specular-right)))
        (:-x #((environments helipad-specular-left)))
        (:+y #((environments helipad-specular-top)))
        (:-y #((environments helipad-specular-bottom)))
        (:+z #((environments helipad-specular-front)))
        (:-z #((environments helipad-specular-back))))))))

(c:define-texture pisa (:texture-cube-map-array)
  (:texture-min-filter :linear-mipmap-linear)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name.
   #(((:layout :six)
      #((:+x #((environments pisa-diffuse-right)))
        (:-x #((environments pisa-diffuse-left)))
        (:+y #((environments pisa-diffuse-top)))
        (:-y #((environments pisa-diffuse-bottom)))
        (:+z #((environments pisa-diffuse-front)))
        (:-z #((environments pisa-diffuse-back)))))
     ((:layout :six)
      #((:+x #((environments pisa-specular-right)))
        (:-x #((environments pisa-specular-left)))
        (:+y #((environments pisa-specular-top)))
        (:-y #((environments pisa-specular-bottom)))
        (:+z #((environments pisa-specular-front)))
        (:-z #((environments pisa-specular-back))))))))

(c:define-texture footprint-court (:texture-cube-map-array)
  (:texture-min-filter :linear-mipmap-linear)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name.
   #(((:layout :six)
      #((:+x #((environments footprint-court-diffuse-right)))
        (:-x #((environments footprint-court-diffuse-left)))
        (:+y #((environments footprint-court-diffuse-top)))
        (:-y #((environments footprint-court-diffuse-bottom)))
        (:+z #((environments footprint-court-diffuse-front)))
        (:-z #((environments footprint-court-diffuse-back)))))
     ((:layout :six)
      #((:+x #((environments footprint-court-specular-right)))
        (:-x #((environments footprint-court-specular-left)))
        (:+y #((environments footprint-court-specular-top)))
        (:-y #((environments footprint-court-specular-bottom)))
        (:+z #((environments footprint-court-specular-front)))
        (:-z #((environments footprint-court-specular-back))))))))

(c:define-texture ennis (:texture-cube-map-array)
  (:texture-min-filter :linear-mipmap-linear)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name.
   #(((:layout :six)
      #((:+x #((environments ennis-diffuse-right)))
        (:-x #((environments ennis-diffuse-left)))
        (:+y #((environments ennis-diffuse-top)))
        (:-y #((environments ennis-diffuse-bottom)))
        (:+z #((environments ennis-diffuse-front)))
        (:-z #((environments ennis-diffuse-back)))))
     ((:layout :six)
      #((:+x #((environments ennis-specular-right)))
        (:-x #((environments ennis-specular-left)))
        (:+y #((environments ennis-specular-top)))
        (:-y #((environments ennis-specular-bottom)))
        (:+z #((environments ennis-specular-front)))
        (:-z #((environments ennis-specular-back))))))))

(c:define-texture field (:texture-cube-map-array)
  (:texture-min-filter :linear-mipmap-linear)
  (:data
   ;; TODO: TMAP Fix to accept texture-map name.
   #(((:layout :six)
      #((:+x #((environments field-diffuse-right)))
        (:-x #((environments field-diffuse-left)))
        (:+y #((environments field-diffuse-top)))
        (:-y #((environments field-diffuse-bottom)))
        (:+z #((environments field-diffuse-front)))
        (:-z #((environments field-diffuse-back)))))
     ((:layout :six)
      #((:+x #((environments field-specular-right)))
        (:-x #((environments field-specular-left)))
        (:+y #((environments field-specular-top)))
        (:-y #((environments field-specular-bottom)))
        (:+z #((environments field-specular-front)))
        (:-z #((environments field-specular-back))))))))


(c:define-texture brdf-lut (:texture-2d)
  (:flip-y t) ;; TODO: Hrm.... is this right?
  (:data
   ;; TODO: TMAP Fix to accept texture-map name.
   #((c:textures c::brdf-lut))))

;;; Materials

(c:define-material damaged-helmet
  (:shader ex/shd:damaged-helmet
   :profiles (x:u-mvp)
   :uniforms
   ((:light.direction (v3:vec -0.7399f0 -0.6428f0 -0.1983f0))
    (:light.color (v3:ones))
    (:light.intensity 2)
    (:sampler 'damaged-helmet/mesh)
    (:base-color-factor (v4:ones))
    (:metallic-factor 1)
    (:roughness-factor 1)
    (:normal-scale 1)
    (:normal-matrix (m3:id))
    (:occlusion-strength 1)
    (:emissive-factor 1)
    (:brdf-lut 'brdf-lut)
    (:environment-sampler 'helipad)
    (:use-punctual t)
    (:use-ibl t))))

;; NOTE: This simple mouse rotator will NOT work correctly if the actor's
;; transform is having other updates applied to it over time. This includes
;; :rotate/velocity, etc. In order to make this possible, we'd need to shove a
;; parent actor onto the model you want to rotate and put this component on THAT
;; one.
(c:define-component simple-mouse-rotator ()
  ((%rot-speed :accessor rot-speed
               :initarg :rot-speed
               :initform .005f0)
   (%orig-orient :accessor orig-orient
                 :initarg :orig-orient
                 :initform nil)
   (%start-drag-point :accessor start-drag-point
                      :initarg :start-drag-point
                      :initform (v2:vec 0f0 0f0))
   (%drag-point :accessor drag-point
                :initarg :drag-point
                :initform (v2:vec 0f0 0f0 ))
   (%end-drag-point :accessor end-drag-point
                    :initarg :end-drag-point
                    :initform (v2:vec 0f0 0f0))
   (%rv :accessor rv
        :initarg :rv
        :initform nil)
   (%clamp-p :accessor clamp-p
             :initarg :clamp-p
             :initform t)
   (%dragging :accessor dragging
              :initarg :dragging
              :initform nil)))

(defmethod c:on-component-update ((self simple-mouse-rotator))

  ;; TODO: This section should be part of a separate core component, that users
  ;; can attach to any camera that want to pick with. This implies that core
  ;; needs a mapping from camera to last-actor-picked.
  (let ((context (c:context self)))
    (cond
      ((c:on-button-enter context :mouse :left)
       (c::pick-actor context (make-instance 'c::line-segment)))
      ((c:on-button-exit context :mouse :left)
       (c::unpick-actor context))))

  (when (or (c::actor-picked-p (c:actor self))
            (dragging self))
    (with-accessors ((rot-speed rot-speed)
                     (orig-orient orig-orient)
                     (start-drag-point start-drag-point)
                     (drag-point drag-point)
                     (end-drag-point end-drag-point)
                     (dragging dragging)
                     (rv rv)
                     (clamp-p clamp-p)
                     (context context))
        self
      (u:mvlet* ((context (c:context self))
                 (x y (c:get-mouse-position context))
                 (lm-start-drag-p (c:on-button-enter context :mouse :left))
                 (lm-stop-drag-p (c:on-button-exit context :mouse :left))
                 (range (- o:pi/2 .001f0)))
        (when (or (null x) (null y))
          ;; TODO: Figure out how this even happens.
          (return-from c:on-component-update))
        ;; TODO: Drag detection and handling is very primitive and prolly
        ;; should be done elsewhere.  NOTE: We get this ONCE and then the
        ;; entire rotation of the object is dynamically built as an persistent
        ;; orientation offset from this origin orientation.
        (unless orig-orient
          (setf orig-orient (c:get-rotation self)))
        (unless rv
          ;; RV represents a persistent 2D point we'll be moving around with
          ;; the mouse--even across multiple drag events. This RV 2D point
          ;; represents the persistent orientation difference we're going to
          ;; apply to the original-orientation
          (setf rv (v2:copy start-drag-point)))
        (when lm-start-drag-p
          (setf start-drag-point (v2:vec* x y)
                drag-point (v2:vec* x y)
                dragging t))
        (when dragging
          (setf drag-point (v2:vec* x y))
          (let* ((dv (v2:- drag-point start-drag-point)))
            ;; TODO: This mathematical concept here is slightly clunky, so
            ;; fixup the transform API to make this a lot easier to do.
            (let* (;; This is built by adding the new drag vector to RV. Sirnce
                   ;; RV represents (as a 2d point) the offset from the
                   ;; original orientation, when a new drag event happens it'll
                   ;; smoothly start from RV in the new drag.
                   (x-rot (+ (v2:x rv) (* (v2:x dv) rot-speed)))
                   (y-rot (- (+ (v2:y rv) (* (v2:y dv) rot-speed))))
                   (y-rot (if clamp-p (u:clamp y-rot (- range) range) y-rot))
                   (dv-rot (q:orient :local
                                     :y x-rot
                                     :x y-rot))
                   ;; Create the new potential rotation starting from the
                   ;; original orientation that takes into consideration the
                   ;; new orientation indicated by dv-rot
                   (putative-rot (q:rotate dv-rot orig-orient)))
              ;; Now, preview it to the user (remember, the orig-orientation
              ;; was the orientation BEFORE the dragging started). So when the
              ;; user lets go of the LMB, this BECOMES the new orientation for
              ;; the next drag attempt.
              (c:rotate self putative-rot :replace t))))
        (when lm-stop-drag-p
          (setf dragging nil
                end-drag-point (v2:vec* x y))
          ;; Update RV to its final position at the end of the drag wrt where
          ;; RV used to be.  This allows the NEXT drag to start at the same
          ;; place the previous drag ended.
          (let* ((dv (v2:- end-drag-point start-drag-point)))
            (v2:with-components ((r rv) (d dv))
              (incf rx (* dx rot-speed))
              (incf ry (* dy rot-speed))
              (when clamp-p
                (setf ry (u:clamp ry (- range) range))))))))))

;;; Prefabs

;; TODO: Add a component to swap out matcap and real texture with a key strike.
(c:define-prefab "default-helmet" (:library examples)
  "A base description of the damaged helmet so we can use it easily elsewhere."
  ("model"
   (comp:transform
    :rotate (q:orient :local :x o:pi/2))
   (comp:mesh :asset '(meshes damaged-helmet)
              :name "helmet")
   (comp:render :material 'damaged-helmet
                :slave (c:ref :self :component 'comp:mesh))))

(c:define-prefab "damaged-helmet" (:library examples)
  "A simple test to see if the helmet displays properly."
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)
                :free-look t))
  (("helmet" :copy "/default-helmet")
   (comp:transform :rotate/velocity (v3:velocity v3:+up+ o:pi/6)
                   :scale 17f0)))

(c:define-prefab "damaged-helmet-group" (:library examples)
  "Test to ensure that the reflections of both helmet's look correct.
There used to be a bug where they wouldn't update properly. It was obviously
wrong."
  (("camera" :copy "/cameras/perspective"))
  (("helmet1" :copy "/default-helmet")
   (comp:transform :rotate/velocity (v3:velocity (v3:ones) o:pi/3)
                   :translate (v3:vec -15f0 0f0 0f0)
                   :scale 15f0))
  (("helmet2" :copy "/default-helmet")
   (comp:transform :translate (v3:vec 15f0 0f0 0f0)
                   :scale 15f0)))

(c:define-prefab "damaged-helmet-interactive" (:library examples)
  (("helmet" :copy "/default-helmet")
   (comp:sphere :on-layer :ground)
   (simple-mouse-rotator :clamp-p t)))

(c:define-prefab "damaged-helmet-turn-table" (:library examples)
  "Move the helmet with the mouse!"
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)))
  (("helmet" :copy "/damaged-helmet-interactive")
   (comp:transform :scale 17f0)))

(c:define-prefab "damaged-helmet-picking-turn-table" (:library examples)
  "Move the helmet with the mouse!"
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)))
  (("helmet1" :copy "/damaged-helmet-interactive")
   (comp:transform :translate (v3:vec -15f0 0f0 0f0)
                   :scale 14f0))
  (("helmet2" :copy "/damaged-helmet-interactive")
   (comp:transform :translate (v3:vec 15f0 0f0 0f0)
                   :scale 14f0)))

(c:define-prefab "flying-helmet" (:library examples)
  "A helmet flies face first while turning to (its) left in a circle."
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args)
                :free-look t))
  (("helmet" :copy "/default-helmet")
   (comp:transform
    :rotate/velocity (v3:velocity v3:+up+ o:pi/6)
    :translate/velocity (v3:velocity v3:+forward+ 1f0)
    :scale 7f0)))
