(in-package :fl.core)

(defstruct input-state enter enabled leave)

(defun input-transition-in (core-state input)
  (symbol-macrolet ((state (au:href (states (input-data core-state)) input)))
    (with-slots (enter enabled leave) state
      (if state
          (setf enter t enabled t leave nil)
          (setf state (make-input-state :enter t :enabled t)))
      (push input (entering (input-data core-state))))))

(defun input-transition-out (core-state input)
  (au:when-let ((state (au:href (states (input-data core-state)) input)))
    (with-slots (enter enabled leave) state
      (setf enter nil enabled nil leave t)
      (push input (leaving (input-data core-state))))))

(defun enable-entering (core-state)
  (symbol-macrolet ((entering (entering (input-data core-state))))
    (dolist (input entering)
      (with-slots (enter enabled leave) (au:href (states (input-data core-state)) input)
        (setf enter nil enabled t leave nil)))
    (setf entering nil)))

(defun disable-leaving (core-state)
  (symbol-macrolet ((leaving (leaving (input-data core-state))))
    (dolist (input leaving)
      (with-slots (enter enabled leave) (au:href (states (input-data core-state)) input)
        (setf enter nil enabled nil leave nil)))
    (setf leaving nil)))

(defun input-state-enter-p (context group button)
  (au:when-let ((state (au:href (states (input-data (core-state context))) (cons group button))))
    (input-state-enter state)))

(defun input-state-enabled-p (context group button)
  (au:when-let ((state (au:href (states (input-data (core-state context))) (cons group button))))
    (input-state-enabled state)))

(defun input-state-leave-p (context group button)
  (au:when-let ((state (au:href (states (input-data (core-state context))) (cons group button))))
    (input-state-leave state)))
