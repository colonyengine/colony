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
         (au:mvlet* ((lx ly (get-gamepad-analog context '(:gamepad1 :left-stick)))
                     (rx ry (get-gamepad-analog context '(:gamepad1 :right-stick))))
           (let* ((vec (v3:make lx ly 0))
                  (vec-magnitude (v3:magnitude vec))
                  (largest-vec (v3:make 1 1 0))
                  (largest-vec-magnitude (v3:magnitude largest-vec))
                  (vec-normalized-maybe (if (> vec-magnitude 1)
                                            (v3:normalize vec)
                                            vec))
                  (vec (v3:scale vec-normalized-maybe 150.0)))
             (fl.comp:translate %transform vec :replace-p t))
           (unless (= rx ry 0.0)
             (let ((angle (atan (- rx) ry)))
               (fl.comp:rotate %transform (v3:make 0 0 angle) :replace-p t)))))))))
