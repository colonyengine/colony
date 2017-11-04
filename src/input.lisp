(in-package :gear)

(defmethod kit.sdl2:keyboard-event ((display display) state ts repeatp keysym)
  (let ((key (sdl2:scancode keysym)))
    (unless repeatp
      (case state
        (:keydown (key-down display key))
        (:keyup (key-up display key))))))

(defgeneric key-down (display key)
  (:method ((display display) key))
  (:method :around ((display display) key)
    (let ((key-name (sdl2:scancode-key-name key)))
      (format t "Key pressed: ~a~%" key-name)
      (call-next-method))))

(defgeneric key-up (display key)
  (:method ((display display) key))
  (:method :around ((display display) key)
    (let ((key-name (sdl2:scancode-key-name key)))
      (format t "Key released: ~a~%" key-name)
      (call-next-method))))
