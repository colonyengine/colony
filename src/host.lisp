(in-package :fl.host)

;;;; STAY AWAY FROM THIS FILE UNTIL FINISHED

;;; Protocol

(defgeneric initialize-host (host &key &allow-other-keys)
  (:method (host &key)
    (error "Host ~s does not implement INITIALIZE-HOST." host))
  (:documentation "Performs any steps needed to initialize the host system."))

(defgeneric shutdown-host (host &key &allow-other-keys)
  (:method (host &key)
    (error "Host ~s does not implement SHUTDOWN-HOST." host))
  (:documentation "Performs any steps needed to finalize the shutdown of the host system."))

(defgeneric make-window (host &key &allow-other-keys)
  (:method (host &key)
    (error "Host ~s does not implement MAKE-WINDOW." host))
  (:documentation "Creates a display window on the host."))

(defgeneric make-opengl-context (host window &key &allow-other-keys)
  (:method (host window &key)
    (error "Host ~s does not implement MAKE-OPENGL-CONTEXT." host))
  (:documentation "Creates an OpenGL context for the host window."))

(defgeneric close-window (host window &key &allow-other-keys)
  (:method (host window &key)
    (error "Host ~s does not implement CLOSE-WINDOW." host))
  (:documentation "Closes a display window."))

(defgeneric window-size (host window &key &allow-other-keys)
  (:method (host window &key)
    (error "Host ~s does not implement WINDOW-SIZE." host))
  (:documentation "Get the size of a display window, as a list of 2 elements."))

(defgeneric resize-window (host window width height &key &allow-other-keys)
  (:method (host window width height &key)
    (error "Host ~s does not implement RESIZE-WINDOW." host))
  (:documentation "Request the host to resize a window."))

(defgeneric window-fullscreen (host window &key &allow-other-keys)
  (:method (host window &key)
    (error "Host ~s does not implement WINDOW-FULLSCREEN." host))
  (:documentation "Request the host to put a display window into fullscreen mode."))
