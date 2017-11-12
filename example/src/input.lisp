(in-package :first-light-example)

(defmethod key-down ((display display) (key (eql :scancode-escape)))
  (quit-engine display))
