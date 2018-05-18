(in-package :fl.example.mfiano)

(defmethod key-down ((display display) (key (eql :scancode-escape)))
  (stop-engine (core-state display)))
