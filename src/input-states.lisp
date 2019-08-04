(in-package #:virality.input)

(defstruct input-state enter enabled exit)

(defun input-transition-in (data input)
  (symbol-macrolet ((state (u:href (states data) input)))
    (with-slots (enter enabled exit) state
      (if state
          (setf enter t
                enabled t
                exit nil)
          (setf state (make-input-state :enter t :enabled t)))
      (push input (entering data)))))

(defun input-transition-out (data input)
  (a:when-let ((state (u:href (states data) input)))
    (with-slots (enter enabled exit) state
      (setf enter nil
            enabled nil
            exit t)
      (push input (exiting data)))))

(defun enable-entering (data)
  (symbol-macrolet ((entering (entering data)))
    (dolist (input entering)
      (with-slots (enter enabled exit) (u:href (states data) input)
        (setf enter nil
              enabled t
              exit nil)))
    (setf entering nil)))

(defun disable-exiting (data)
  (symbol-macrolet ((exiting (exiting data)))
    (dolist (input exiting)
      (with-slots (enter enabled exit) (u:href (states data) input)
        (setf enter nil
              enabled nil
              exit nil)))
    (setf exiting nil)))

(defun input-enter-p (context input)
  (a:when-let* ((data (v::input-data (v::core context)))
                (state (u:href (states data) input)))
    (input-state-enter state)))

(defun input-enabled-p (context input)
  (a:when-let* ((data (v::input-data (v::core context)))
                (state (u:href (states data) input)))
    (input-state-enabled state)))

(defun input-exit-p (context input)
  (a:when-let* ((data (v::input-data (v::core context)))
                (state (u:href (states data) input)))
    (input-state-exit state)))
