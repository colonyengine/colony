(in-package #:virality)

(u:eval-always
  (defun get-config-option-name (key)
    (a:format-symbol :virality "=~a=" key)))

(defun validate-config-options (options)
  (u:do-plist (k v options)
    (u:unless-found (x (u:href =meta/config/default= k))
      (error "~s is not a valid config option. Valid options:~%~{~s~%~}"
             x (u:hash-keys =meta/config/default=)))))

(defun load-config (project-name &rest args)
  (u:do-hash (k v (u:href =meta/config/project= project-name))
    (let ((global (get-config-option-name k)))
      (setf (symbol-value global) v)))
  (u:do-plist (k v args)
    (let ((global (get-config-option-name k)))
      (setf (symbol-value global) v))))

(defmacro define-config (project-name options &body body)
  (declare (ignore options))
  `(progn
     ,@(if (eq project-name :virality)
           `((setf =meta/config/default= (u:dict ,@body))
             ,@(loop :for (k v) :on body :by #'cddr
                     :for name = (get-config-option-name k)
                     :append `((global-vars:define-global-parameter ,name ,v)
                               (export ',name))))
           `((validate-config-options ',body)
             (setf (u:href =meta/config/project= ,project-name)
                   (u:hash-merge =meta/config/default= (u:dict ,@body)))))))

(define-config :virality ()
  :allow-screensaver nil
  :anti-alias-level 4
  :delta 1/30
  :debug-interval 5
  :initial-scene nil
  :log-level :debug
  :log-repl-enabled t
  :log-repl-categories '(:virality)
  :opengl-version "4.3"
  :period-interval 0.25
  :release nil
  :threads 4
  :vsync t
  :window-height 450
  :window-width 800
  :window-title "Virality Engine")
