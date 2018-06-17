(in-package :%fl.core)

(au:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore :mouse-focus-enter
      :mouse-focus-exit :keyboard-focus-enter :keyboard-focus-exit :close nil nil)
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

(defun on-window-show (core-state)
  (declare (ignore core-state)))

(defun on-window-hide (core-state)
  (declare (ignore core-state)))

(defun on-window-move (core-state &key x y)
  (declare (ignore core-state x y)))

(defun on-window-resize (core-state &key width height)
  (declare (ignore core-state width height)))

(defun on-window-minimize (core-state)
  (declare (ignore core-state)))

(defun on-window-maximize (core-state)
  (declare (ignore core-state)))

(defun on-window-restore (core-state)
  (declare (ignore core-state)))

(defun on-window-mouse-focus-enter (core-state)
  (declare (ignore core-state)))

(defun on-window-mouse-focus-exit (core-state)
  (declare (ignore core-state)))

(defun on-window-keyboard-focus-enter (core-state)
  (declare (ignore core-state)))

(defun on-window-keyboard-focus-exit (core-state)
  (declare (ignore core-state)))

(defun on-window-close (core-state)
  (declare (ignore core-state)))
