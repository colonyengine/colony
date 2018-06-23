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
  (when (or (input-enter-p context '(:gamepad1 :a))
            (input-enter-p context '(:mouse :mouse-left)))

    (let ((actor (%fl::make-actor context :id (au:unique-name 'shot))))
      (%fl:attach-component
       actor (%fl:make-component 'fl.comp:transform context
                                 :scale/current (v3:make 1 1 1)))
      (%fl:attach-component
       actor (%fl:make-component 'sprite-sheet
                                 context
                                 :spec-path '(:local "data/sprites.sexp")
                                 :material 'fl.mfiano.materials::sprite
                                 :animations (make-sprite-sheet-animations
                                              0 0 #(#(1 "ship11")))))

      ;; For now, I recommend after spawning, to immediately destroy it and
      ;; supply the TTL at that time. Don't specify :ttl in make-actor.
      (%fl:spawn-actor actor context)
      (%fl::destroy actor context :ttl 1))))
