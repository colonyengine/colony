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

(define-component shot-emitter () ())

(defmethod initialize-component ((component shot-emitter) (context context)))

(defmethod update-component ((component shot-emitter) (context context))
  (when (input-enter-p context '(:gamepad1 :a))
    (let ((actor (make-actor context))
          (transform (make-component 'fl.comp:transform context))
          (sprite (make-component 'sprite-sheet
                                  context
                                  :spec-path '(:local "data/sprites.sexp")
                                  :material 'fl.mfiano.materials::sprite
                                  :animations (make-sprite-sheet-animations
                                               0 0 #(#(1 "ship15"))))))
      (attach-multiple-components actor transform sprite)
      (spawn-actor actor context))))
