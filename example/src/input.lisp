(in-package :gear-example)

(defmethod key-down ((display display) (key (eql :scancode-escape)))
  (quit display))
