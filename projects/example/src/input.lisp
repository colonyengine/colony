(in-package :fl.example)

(defmethod key-down ((display display) (key (eql :scancode-escape)))
  (stop-engine (core-state display)))
