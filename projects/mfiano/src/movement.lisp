(in-package :fl.mfiano)

(define-component simple-movement ()
  ((transform :default nil)
   (velocity :default 1)
   (input-method :default :analog)))

(defmethod initialize-component ((component simple-movement) (context context))
  (with-accessors ((actor actor) (transform transform)) component
    (setf transform (actor-component-by-type actor 'transform))))

(defmethod update-component ((component simple-movement) (context context))
  (with-slots (%transform %velocity %input-method) component
    (let ((velocity (* %velocity (delta context))))
      (case %input-method
        (:keyboard
         (when (input-enabled-p context '(:key :w))
           (fl.comp:translate %transform (v3:scale (v3:make 0 1 0) velocity)))
         (when (input-enabled-p context '(:key :s))
           (fl.comp:translate %transform (v3:scale (v3:make 0 -1 0) velocity)))
         (when (input-enabled-p context '(:key :a))
           (fl.comp:translate %transform (v3:scale (v3:make -1 0 0) velocity)))
         (when (input-enabled-p context '(:key :d))
           (fl.comp:translate %transform (v3:scale (v3:make 1 0 0) velocity))))
        (:dpad
         (when (input-enabled-p context '(:gamepad1 :up))
           (fl.comp:translate %transform (v3:scale (v3:make 0 1 0) velocity)))
         (when (input-enabled-p context '(:gamepad1 :down))
           (fl.comp:translate %transform (v3:scale (v3:make 0 -1 0) velocity)))
         (when (input-enabled-p context '(:gamepad1 :left))
           (fl.comp:translate %transform (v3:scale (v3:make -1 0 0) velocity)))
         (when (input-enabled-p context '(:gamepad1 :right))
           (fl.comp:translate %transform (v3:scale (v3:make 1 0 0) velocity))))
        (:analog
         (let ((x (get-gamepad-axis context :gamepad1 :left-horizontal))
               (y (get-gamepad-axis context :gamepad1 :left-vertical)))
           (fl.comp:translate %transform (v3:scale (v3:make x y 0) velocity))))))))
