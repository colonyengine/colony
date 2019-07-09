(in-package #:%first-light)

(setf (%fl:meta 'options/default)
      (u:dict :title "First-Light Game Project"
              :window-width 800
              :window-height 450
              :delta 1/30
              :vsync :on
              :periodic-interval 0.25
              :debug-interval 5
              :log-repl-enabled t
              :log-repl-categories '(:fl)
              :log-level :debug
              :anti-alias-level 4
              :opengl-version "4.3"
              :initial-scene nil))

(defun load-options (core)
  (let ((user-options-path (uiop:merge-pathnames*
                            #p"first-light/first-light.conf"
                            #+windows #p"AppData/Local/"
                            #-windows (uiop:xdg-config-home))))
    (when (uiop:file-exists-p user-options-path)
      (setf (%fl:meta 'options/user)
            (apply #'u:dict
                   (u:safe-read-file-forms
                    user-options-path))))
    (setf (options core)
          (u:hash-merge (%fl:meta 'options/default)
                        (or (%fl:meta 'options/project)
                            (u:dict))
                        (or (%fl:meta 'options/user)
                            (u:dict))))))

(defun option (context option-name)
  (u:href (options context) option-name))

(defmacro define-options (() &body body)
  `(setf (%fl:meta 'options/project) (u:dict ,@body)))
