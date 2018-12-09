(in-package :%fl)

(defstruct input-state enter enabled exit)

(defun input-transition-in (core-state input)
  (symbol-macrolet ((state (fu:href (states (input-data core-state)) input)))
    (with-slots (enter enabled exit) state
      (if state
          (setf enter t enabled t exit nil)
          (setf state (make-input-state :enter t :enabled t)))
      (push input (entering (input-data core-state))))))

(defun input-transition-out (core-state input)
  (fu:when-let ((state (fu:href (states (input-data core-state)) input)))
    (with-slots (enter enabled exit) state
      (setf enter nil enabled nil exit t)
      (push input (exiting (input-data core-state))))))

(defun enable-entering (core-state)
  (symbol-macrolet ((entering (entering (input-data core-state))))
    (dolist (input entering)
      (with-slots (enter enabled exit) (fu:href (states (input-data core-state)) input)
        (setf enter nil enabled t exit nil)))
    (setf entering nil)))

(defun disable-exiting (core-state)
  (symbol-macrolet ((exiting (exiting (input-data core-state))))
    (dolist (input exiting)
      (with-slots (enter enabled exit) (fu:href (states (input-data core-state)) input)
        (setf enter nil enabled nil exit nil)))
    (setf exiting nil)))

(defun input-enter-p (context input)
  (fu:when-let ((state (fu:href (states (input-data (core-state context))) input)))
    (input-state-enter state)))

(defun input-enabled-p (context input)
  (fu:when-let ((state (fu:href (states (input-data (core-state context))) input)))
    (input-state-enabled state)))

(defun input-exit-p (context input)
  (fu:when-let ((state (fu:href (states (input-data (core-state context))) input)))
    (input-state-exit state)))
