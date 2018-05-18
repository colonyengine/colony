(in-package :fl.mfiano)

(defmethod key-down ((display display) (key (eql :scancode-escape)))
  (stop-engine (core-state display)))
