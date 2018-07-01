(in-package :fl.mfiano)

(define-component simple-movement ()
  ((transform :default nil)))

(defmethod initialize-component ((component simple-movement) (context context))
  (with-accessors ((actor actor) (transform transform)) component
    (setf transform (actor-component-by-type actor 'transform))))

(defmethod update-component ((component simple-movement) (context context))
  (with-slots (%transform) component
    (au:mvlet* ((lx ly (get-gamepad-analog context '(:gamepad1 :left-stick)))
                (rx ry (get-gamepad-analog context '(:gamepad1 :right-stick))))
      (let ((vec (v3:make lx ly 0)))
        (v3:scale! vec (if (> (v3:magnitude vec) 1) (v3:normalize vec) vec) 150.0)
        (fl.comp:translate %transform (v3:+ (v3:make -400 0 0) vec) :replace-p t))
      (unless (= rx ry 0.0)
        (let* ((angle (atan (- rx) ry))
               ;; keep angle from 0 to 2pi for easier debugging of other things.
               (angle (if (< angle 0)
                          (+ pi (- pi (abs angle)))
                          angle)))
          (fl.comp:rotate %transform (v3:make 0 0 angle) :replace-p t))))))






(define-component shot-mover ()
  ((transform :default nil)
   (velocity :default 0)))

(defmethod initialize-component ((component shot-mover) (context context))
  (setf (transform component)
        (actor-component-by-type (actor component) 'transform)))


(defmethod update-component ((component shot-mover) (context context))
  (fl.comp:translate
   (transform component)
   ;; fly along the forward axis for this shot
   (let* ((a (m4:rotation-axis-to-vec3
              (fl.comp:local (transform component)) :y))
          (b  (v3:normalize a))
          (move-delta (* (velocity component) (frame-time context))))
     (v3:scale b move-delta))))


(define-component shot-emitter ()
  ((emitter-transform :default nil)))

(defmethod initialize-component ((component shot-emitter) (context context))
  (setf (emitter-transform component)
        (actor-component-by-type (actor component) 'transform)))

(defmethod update-component ((component shot-emitter) (context context))
  (when (or (input-enter-p context '(:gamepad1 :right-shoulder))
            (input-enter-p context '(:mouse :mouse-left)))

    (let* ((parent-model (fl.comp:model (emitter-transform component)))
           (parent-translation (m4:translation-to-vec3 parent-model))
           (parent-rotation
             ;; TODO: The model matrix has scaling on it, which fucks up the
             ;; quat conversion algorithm. So we test getting rid of it here.
             ;;
             ;; This must be fixed by going into gamebox-math/src/quat.lisp and
             ;; adding a "&key (normalize t)" argument to the function.  Then,
             ;; when normalize is t (the default), it temporarily normalizes the
             ;; basis vectors and uses those instead. Then the from-quat4
             ;; conversion will work regardless of scaling on the transform.
             (let* ((x-rot (m4:rotation-axis-to-vec3 parent-model :x))
                    (x-rot (v3:normalize! x-rot x-rot))
                    (y-rot (m4:rotation-axis-to-vec3 parent-model :y))
                    (y-rot (v3:normalize! y-rot y-rot))
                    (z-rot (m4:rotation-axis-to-vec3 parent-model :z))
                    (z-rot (v3:normalize! z-rot z-rot))
                    (unscaled-mat (m4:id)))
               (m4:rotation-axis-from-vec3! unscaled-mat x-rot :x)
               (m4:rotation-axis-from-vec3! unscaled-mat y-rot :y)
               (m4:rotation-axis-from-vec3! unscaled-mat z-rot :z)
               ;; finally get the rotation.
               ;; with the above suggested fix to gamebox-math, this call will
               ;; just do the right thing and can be replaced with
               ;; (quat:from-mat4 parent-model)
               (quat:from-mat4 unscaled-mat)))

           ;; here is the code to make the new shot.
           (new-actor (%fl::make-actor context :id (au:unique-name 'shot)))
           (transform (make-component 'fl.comp:transform context
                                      ;; here I init the local location/rotation
                                      ;; to semantically match the emitter
                                      ;; actor.
                                      :translation/current parent-translation
                                      :rotation/current parent-rotation))
           (shot-mover (make-component 'shot-mover context
                                       :velocity 1000))
           (sprite (make-component 'sprite-sheet context
                                   :spec-path '(:local "data/sprites.sexp")
                                   :material 'fl.mfiano.materials::sprite
                                   :animations (make-sprite-sheet-animations
                                                0 0 #(#(.25
                                                        "bullet01"
                                                        "bullet02"
                                                        ))))))

      (attach-multiple-components new-actor transform shot-mover sprite)

      ;; The shot is free in the universe.
      (spawn-actor new-actor context)

      ;; This is the method for destroying actors and components. Add to public
      ;; API. Don't use :ttl in the make-actor call yet.
      (%fl::destroy new-actor context :ttl 1))))
