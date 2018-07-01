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
        (let ((angle (atan (- rx) ry)))
          (fl.comp:rotate %transform (v3:make 0 0 angle) :replace-p t))))))






(define-component shot-mover ()
  ((transform :default nil)
   (velocity :default (v3:make 0 0 0))))

(defmethod initialize-component ((component shot-mover) (context context))
  (setf (transform component)
        (actor-component-by-type (actor component) 'transform)))

(defmethod update-component ((component shot-mover) (context context))
  (fl.comp:translate (transform component) (velocity component)))




(define-component shot-emitter ())

(defmethod initialize-component ((component shot-emitter) (context context)))

(defmethod update-component ((component shot-emitter) (context context))
  (when (or (input-enter-p context '(:gamepad1 :a))
            (input-enter-p context '(:mouse :mouse-left)))

    (let ((actor (%fl::make-actor context :id (au:unique-name 'shot)))
          (transform (make-component 'fl.comp:transform context))
          (shot-mover (make-component
                       'shot-mover context
                       :velocity (v3:make 0 (* 1000 (frame-time context)) 0)))
          (sprite (make-component 'sprite-sheet context
                                  :spec-path '(:local "data/sprites.sexp")
                                  :material 'fl.mfiano.materials::sprite
                                  :animations (make-sprite-sheet-animations
                                               0 0 #(#(.25
                                                       "bullet01"
                                                       "bullet02"
                                                       ))))))

      (attach-multiple-components actor transform shot-mover sprite)

      (spawn-actor actor context :parent (actor component))


      ;; This is the method for destroying actors and components. Add to public
      ;; API. Don't use :ttl in the make-actor call yet.
      (%fl::destroy actor context :ttl 1))))
