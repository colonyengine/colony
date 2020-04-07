(in-package #:virality.examples)

;;; Textures

(v:define-texture damaged-helmet/metallic-roughness (:texture-2d)
  (:data #((:damaged-helmet-textures "metal-roughness.tiff"))))

(v:define-texture damaged-helmet/color (:texture-2d)
  (:data #((:damaged-helmet-textures "albedo.tiff"))))

(v:define-texture damaged-helmet/normal (:texture-2d)
  (:data #((:damaged-helmet-textures "normal.tiff"))))

(v:define-texture damaged-helmet/ambient-occlusion (:texture-2d)
  (:data #((:damaged-helmet-textures "ao.tiff"))))

(v:define-texture damaged-helmet/emissive (:texture-2d)
  (:data #((:damaged-helmet-textures "emissive.tiff"))))

;;; Materials

(v:define-material damaged-helmet
  (:shader ex/shd:damaged-helmet
   :profiles (x/mat:u-mvp)
   :uniforms
   ((:metallic-roughness-values (v2:vec 1))
    (:metallic-roughness-sampler 'damaged-helmet/metallic-roughness)
    (:base-color-sampler 'damaged-helmet/color)
    (:base-color-factor (v4:vec 1))
    (:normal-sampler 'damaged-helmet/normal)
    (:normal-scale 1f0)
    ;; NOTE: This vector points TOWARDS the light.
    (:light-direction (v3:vec 0f0 1f0 1f0))
    (:light-color (v3:vec 1))
    (:occlusion-sampler 'damaged-helmet/ambient-occlusion)
    (:occlusion-strength 1f0)
    (:emissive-sampler 'damaged-helmet/emissive)
    (:emissive-factor 0.3f0))))


;; NOTE: This simple mouse rotator will NOT work correctly if the actor's
;; transform is having other updates applied to it over time. This includes
;; :rotate/velocity or action components that rotate, etc, etc. In order to make
;; this possible, we'd need to shove a parent actor onto the model you want to
;; rotate and put this component on THAT one.
(v:define-component simple-mouse-rotator ()
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

(defmethod v:on-component-update ((self simple-mouse-rotator))
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
    (u:mvlet* ((context (v:context self))
               (x y (v:get-mouse-position context))
               (lm-start-drag-p (v:input-enter-p context '(:mouse :left)))
               (lm-stop-drag-p (v:input-exit-p context '(:mouse :left)))
               (xform (v:component-by-type (v:actor self) 'c/xform:transform))
               (range (- o:pi/2 .001)))

      (when (or (null x) (null y))
        ;; TODO: Figure out how this even happens.
        (return-from v:on-component-update))

      ;; TODO: Drag detection and handling is very primitive and prolly should
      ;; be done elsewhere.

      ;; NOTE: We get this ONCE and then the entire rotation of the object is
      ;; dynamically built as an persistent orientation offset from this origin
      ;; orientation.
      (unless orig-orient
        (setf orig-orient (c/xform:get-rotation xform :copy t)))

      (unless rv
        ;; RV represents a persistent 2D point we'll be moving around with the
        ;; mouse--even across multiple drag events. This RV 2D point represents
        ;; the persistent orientation difference we're going to apply to the
        ;; original-orientation
        (setf rv (v2:copy start-drag-point)))

      (when lm-start-drag-p
        (setf start-drag-point (v2:vec x y)
              drag-point (v2:vec x y)
              dragging t))

      (when dragging
        (setf drag-point (v2:vec x y))

        (let* ((dv (v2:- drag-point start-drag-point)))
          ;; TODO: This mathematical concept here is slightly clunky, so fixup
          ;; the transform API to make this a lot easier to do.

          (let* (;; This is built by adding the new drag vector to RV. Sirnce
                 ;; RV represents (as a 2d point) the offset from the
                 ;; original orientation, when a new drag event happens it'll
                 ;; smoothly start from RV in the new drag.
                 (x-rot (+ (v2:x rv) (* (v2:x dv) rot-speed)))
                 (y-rot (+ (v2:y rv) (* (v2:y dv) rot-speed)))
                 (y-rot (if clamp-p (a:clamp y-rot (- range) range) y-rot))
                 (dv-rot (q:orient :local
                                   :y x-rot
                                   :x y-rot
                                   ))
                 ;; Create the new potential rotation starting from the
                 ;; original orientation that takes into consideration the
                 ;; new orientation indicated by dv-rot
                 (putative-rot (q:rotate dv-rot orig-orient)))

            ;; Now, preview it to the user (remember, the orig-orientation was
            ;; the orientation BEFORE the dragging started). So when the user
            ;; lets go of the LMB, this BECOMES the new orientation for the
            ;; next drag attempt.
            (c/xform:rotate xform putative-rot :replace-p t))))

      (when lm-stop-drag-p
        (setf dragging nil
              end-drag-point (v2:vec x y))
        ;; Update RV to its final position at the end of the drag wrt where RV
        ;; used to be.  This allows the NEXT drag to start at the same place the
        ;; previous drag ended.
        (let* ((dv (v2:- end-drag-point start-drag-point)))
          (v2:with-components ((r rv) (d dv))
            (incf rx (* dx rot-speed))
            (incf ry (* dy rot-speed))
            (when clamp-p
              (setf ry (a:clamp ry (- range) range)))))))))



;;; Prefabs

(v:define-prefab "damaged-helmet" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args)
                 :free-look t))
  (("helmet" :copy "/mesh")
   (c/xform:transform :rotate (q:orient :local :x o:pi/2)
                      :rotate/velocity (o:make-velocity
                                        v3:+forward+
                                        (- o:pi/6))
                      :scale 17f0)
   (c/smesh:static-mesh :asset '(:mesh "damaged-helmet.glb"))
   (c/render:render :material 'damaged-helmet)))


(v:define-prefab "damaged-helmet-interactive" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args) :zoom 10f0))

  (("helmet" :copy "/mesh")
   (simple-mouse-rotator :clamp-p t)
   (c/xform:transform :rotate (q:orient :local :x o:pi/2)
                      :scale 4f0)
   (c/smesh:static-mesh :asset '(:mesh "damaged-helmet.glb"))
   (c/render:render :material 'damaged-helmet)))
