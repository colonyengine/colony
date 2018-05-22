(in-package :fl.core)

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
      (simple-logger:emit :input.key.down key-name)
      (call-next-method))))

(defgeneric key-up (display key)
  (:method ((display display) key))
  (:method :around ((display display) key)
    (let ((key-name (sdl2:scancode-key-name key)))
      (simple-logger:emit :input.key.up key-name)
      (call-next-method))))

(defmethod kit.sdl2:mousebutton-event ((display display) state ts button x y)
  (case state
    (:mousebuttondown
     (case button
       (1 (mouse-down-left display x y))
       (2 (mouse-down-middle display x y))
       (3 (mouse-down-right display x y))))
    (:mousebuttonup
     (case button
       (1 (mouse-up-left display x y))
       (2 (mouse-up-middle display x y))
       (3 (mouse-up-right display x y))))))

(defgeneric mouse-down-left (display x y)
  (:method (display x y))
  (:method :around ((display display) x y)
    (simple-logger:emit :input.mouse.down 'left x y)
    (call-next-method)))

(defgeneric mouse-up-left (display x y)
  (:method (display x y))
  (:method :around ((display display) x y)
    (simple-logger:emit :input.mouse.up 'left x y)
    (call-next-method)))

(defgeneric mouse-down-middle (display x y)
  (:method (display x y))
  (:method :around ((display display) x y)
    (simple-logger:emit :input.mouse.down 'middle x y)
    (call-next-method)))

(defgeneric mouse-up-middle (display x y)
  (:method (display x y))
  (:method :around ((display display) x y)
    (simple-logger:emit :input.mouse.up 'middle x y)
    (call-next-method)))

(defgeneric mouse-down-right (display x y)
  (:method (display x y))
  (:method :around ((display display) x y)
    (simple-logger:emit :input.mouse.down 'right x y)
    (call-next-method)))

(defgeneric mouse-up-right (display x y)
  (:method (display x y))
  (:method :around ((display display) x y)
    (simple-logger:emit :input.mouse.up 'right x y)
    (call-next-method)))

(defmethod kit.sdl2:mousewheel-event ((display display) ts x y)
  (when (plusp y)
    (mouse-scroll-up display))
  (when (minusp y)
    (mouse-scroll-down display)))

(defgeneric mouse-scroll-up (display)
  (:method (display))
  (:method :around ((display display))
    (simple-logger:emit :input.scroll.up)
    (call-next-method)))

(defgeneric mouse-scroll-down (display)
  (:method (display))
  (:method :around ((display display))
    (simple-logger:emit :input.scroll.down)
    (call-next-method)))
