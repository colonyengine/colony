(in-package :fl.mfiano)

(defmethod key-down ((display display) (key (eql :scancode-escape)))
  (stop-engine (core-state display)))

;;; HACK: This is a hack. Need to figure out the correct way to transform an object manually at runtime

(defmethod key-down ((display display) (key (eql :scancode-left)))
  (let ((camera (active-camera (context (core-state display)))))
    (v3:+! (fl.comp.transform::current (fl.comp.transform::translation (fl.comp.transform:transform camera)))
           (fl.comp.transform::current (fl.comp.transform::translation (fl.comp.transform:transform camera)))
           (v3:make -20 0 0))))

(defmethod key-down ((display display) (key (eql :scancode-right)))
  (let ((camera (active-camera (context (core-state display)))))
    (v3:+! (fl.comp.transform::current (fl.comp.transform::translation (fl.comp.transform:transform camera)))
           (fl.comp.transform::current (fl.comp.transform::translation (fl.comp.transform:transform camera)))
           (v3:make 20 0 0))))

(defmethod key-down ((display display) (key (eql :scancode-up)))
  (let ((camera (active-camera (context (core-state display)))))
    (v3:+! (fl.comp.transform::current (fl.comp.transform::translation (fl.comp.transform:transform camera)))
           (fl.comp.transform::current (fl.comp.transform::translation (fl.comp.transform:transform camera)))
           (v3:make 0 20 0))))

(defmethod key-down ((display display) (key (eql :scancode-down)))
  (let ((camera (active-camera (context (core-state display)))))
    (v3:+! (fl.comp.transform::current (fl.comp.transform::translation (fl.comp.transform:transform camera)))
           (fl.comp.transform::current (fl.comp.transform::translation (fl.comp.transform:transform camera)))
           (v3:make 0 -20 0))))

(defmethod mouse-scroll-up ((display display))
  (when (active-camera (context (core-state display)))
    (fl.comp.camera:zoom-camera display 1)))

(defmethod mouse-scroll-down ((display display))
  (when (active-camera (context (core-state display)))
    (fl.comp.camera:zoom-camera display -1)))
