(in-package #:virality.engine)

(setf (meta 'options/default)
      (u:dict :title "Virality Engine"
              :window-width 800
              :window-height 450
              :delta 1/30
              :vsync :on
              :period-interval 0.25
              :debug-interval 5
              :log-repl-enabled t
              :log-repl-categories '(:virality)
              :log-level :debug
              :anti-alias-level 4
              :opengl-version "4.3"
              :initial-scene nil
              :threads 4))

(defun load-options (core)
  (let ((project (project core))
        (user-options-path (uiop:merge-pathnames*
                            #p"ViralityEngine/user.conf"
                            (uiop:xdg-config-home))))
    (when (uiop:file-exists-p user-options-path)
      (setf (meta 'options/user)
            (apply #'u:dict (u:safe-read-file-forms user-options-path))))
    (setf (options core)
          (u:hash-merge (meta 'options/default)
                        (or (u:href (meta 'options/project) project) (u:dict))
                        (or (meta 'options/user) (u:dict))))))

(defun option (context option-name)
  (u:href (options context) option-name))

(defmacro define-options (project &body body)
  (let ((options '(meta 'options/project)))
    `(progn
       (unless ,options
         (setf ,options (u:dict)))
       (setf (u:href ,options ,project) (u:dict ,@body)))))
