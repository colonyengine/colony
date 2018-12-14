(in-package :%first-light)

(fl.data:set 'options/default
             (au:dict #'eq
                      :title "First-Light Game Project"
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
                      :opengl-version "4.3"))

(defun load-options (core-state)
  (setf (options core-state)
        (fl.util:merge-tables (fl.data:get 'options/default)
                              (or (fl.data:get 'options/project)
                                  (fl.util:dict #'eq)))))

(defun option (context option-name)
  (fl.util:href (options context) option-name))

(defmacro define-options (() &body body)
  `(fl.data:set 'options/project (fl.util:dict #'eq ,@body)))

