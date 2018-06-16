(in-package :fl.core)

(defstruct button-state enter enabled leave)

(defun button-transition-in (core-state button)
  (symbol-macrolet ((state (au:href (states (input-data core-state)) button)))
    (with-slots (enter enabled leave) state
      (if state
          (setf enter t enabled t leave nil)
          (setf state (make-button-state :enter t :enabled t)))
      (push button (buttons-entering (input-data core-state))))))

(defun button-transition-out (core-state button)
  (au:when-let ((state (au:href (states (input-data core-state)) button)))
    (with-slots (enter enabled leave) state
      (setf enter nil enabled nil leave t)
      (push button (buttons-leaving (input-data core-state))))))

(defun enable-entering-buttons (core-state)
  (symbol-macrolet ((entering (buttons-entering (input-data core-state))))
    (dolist (button entering)
      (with-slots (enter enabled leave) (au:href (states (input-data core-state)) button)
        (setf enter nil enabled t leave nil)))
    (setf entering nil)))

(defun disable-leaving-buttons (core-state)
  (symbol-macrolet ((leaving (buttons-leaving (input-data core-state))))
    (dolist (button leaving)
      (with-slots (enter enabled leave) (au:href (states (input-data core-state)) button)
        (setf enter nil enabled nil leave nil)))
    (setf leaving nil)))

(defun button-state-enter-p (context group button)
  (au:when-let ((state (au:href (states (input-data (core-state context))) (cons group button))))
    (button-state-enter state)))

(defun button-state-enabled-p (context group button)
  (au:when-let ((state (au:href (states (input-data (core-state context))) (cons group button))))
    (button-state-enabled state)))

(defun button-state-leave-p (context group button)
  (au:when-let ((state (au:href (states (input-data (core-state context))) (cons group button))))
    (button-state-leave state)))
