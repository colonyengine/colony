(in-package #:virality.engine)

(setf (meta 'options/default)
      (u:dict :title "Virality Engine Project"
              :window-width 800
              :window-height 450
              :delta 1/30
              :vsync :on
              :periodic-interval 0.25
              :debug-interval 5
              :log-repl-enabled t
              :log-repl-categories '(:virality)
              :log-level :debug
              :anti-alias-level 4
              :opengl-version "4.3"
              :initial-scene nil))

(defun load-options (core)
  (let ((user-options-path (uiop:merge-pathnames*
                            #p"virality-engine/user.conf"
                            (uiop:xdg-config-home))))
    (when (uiop:file-exists-p user-options-path)
      (setf (meta 'options/user)
            (apply #'u:dict
                   (u:safe-read-file-forms
                    user-options-path))))
    (setf (options core)
          (u:hash-merge (meta 'options/default)
                        (or (meta 'options/project)
                            (u:dict))
                        (or (meta 'options/user)
                            (u:dict))))))

(defun option (context option-name)
  (u:href (options context) option-name))

(defmacro define-options (() &body body)
  `(setf (meta 'options/project) (u:dict ,@body)))
