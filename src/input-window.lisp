(in-package #:virality.input)

(a:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore
      :mouse-focus-enter :mouse-focus-exit :keyboard-focus-enter
      :keyboard-focus-exit :close nil nil)
  :test #'equalp)

(defun get-window-title (window)
  (sdl2:get-window-title window))

(defun set-window-title (window title)
  (sdl2:set-window-title window title))

(defun get-window-size (window)
  (multiple-value-list (sdl2:get-window-size window)))

(defun set-window-size (window width height)
  (sdl2:set-window-size window width height))

(defun get-window-mode (window)
  (if (member :fullscreen-desktop (sdl2:get-window-flags window))
      :fullscreen
      :windowed))

(defun set-window-mode (window mode)
  (ecase mode
    (:fullscreen (sdl2:set-window-fullscreen window :desktop))
    (:windowed (sdl2:set-window-fullscreen window :windowed))))

(defun set-window-hidden (window)
  (sdl2:hide-window window))

(defun set-window-visible (window)
  (sdl2:show-window window))

(defun on-window-show (context)
  (declare (ignore context)))

(defun on-window-hide (context)
  (declare (ignore context)))

(defun on-window-move (context &key x y)
  (declare (ignore context x y)))

(defun on-window-resize (context &key width height)
  (declare (ignore context width height)))

(defun on-window-minimize (context)
  (declare (ignore context)))

(defun on-window-maximize (context)
  (declare (ignore context)))

(defun on-window-restore (context)
  (declare (ignore context)))

(defun on-window-mouse-focus-enter (context)
  (declare (ignore context)))

(defun on-window-mouse-focus-exit (context)
  (declare (ignore context)))

(defun on-window-keyboard-focus-enter (context)
  (declare (ignore context)))

(defun on-window-keyboard-focus-exit (context)
  (declare (ignore context)))

(defun on-window-close (context)
  (declare (ignore context)))
