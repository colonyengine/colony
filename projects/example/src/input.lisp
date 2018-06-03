(in-package :fl.example)

(defmethod key-down ((display display) (key (eql :scancode-escape)))
  (stop-engine (core-state display)))

(defmethod mouse-scroll-up ((display display))
  (when (active-camera (context (core-state display)))
    (fl.comp.camera:zoom-camera display 1)))

(defmethod mouse-scroll-down ((display display))
  (when (active-camera (context (core-state display)))
    (fl.comp.camera:zoom-camera display -1)))
