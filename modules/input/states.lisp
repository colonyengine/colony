(in-package #:first-light.input)

(defstruct input-state enter enabled exit)

(defun input-transition-in (input-data input)
  (symbol-macrolet ((state (u:href (states input-data) input)))
    (with-slots (enter enabled exit) state
      (if state
          (setf enter t enabled t exit nil)
          (setf state (make-input-state :enter t :enabled t)))
      (push input (entering input-data)))))

(defun input-transition-out (input-data input)
  (a:when-let ((state (u:href (states input-data) input)))
    (with-slots (enter enabled exit) state
      (setf enter nil enabled nil exit t)
      (push input (exiting input-data)))))

(defun enable-entering (input-data)
  (symbol-macrolet ((entering (entering input-data)))
    (dolist (input entering)
      (with-slots (enter enabled exit) (u:href (states input-data) input)
        (setf enter nil enabled t exit nil)))
    (setf entering nil)))

(defun disable-exiting (input-data)
  (symbol-macrolet ((exiting (exiting input-data)))
    (dolist (input exiting)
      (with-slots (enter enabled exit) (u:href (states input-data) input)
        (setf enter nil enabled nil exit nil)))
    (setf exiting nil)))

(defun input-enter-p (input-data input)
  (a:when-let ((state (u:href (states input-data) input)))
    (input-state-enter state)))

(defun input-enabled-p (input-data input)
  (a:when-let ((state (u:href (states input-data) input)))
    (input-state-enabled state)))

(defun input-exit-p (input-data input)
  (a:when-let ((state (u:href (states input-data) input)))
    (input-state-exit state)))
