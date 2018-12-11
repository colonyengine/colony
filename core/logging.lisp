(in-package :%fl)

(defun enable-logging (core-state)
  (let ((context (context core-state)))
    (with-cfg (log-level log-repl-enabled log-repl-categories) context
      (unless (v:thread v:*global-controller*)
        (v:start v:*global-controller*))
      (when log-repl-enabled
        (setf (v:repl-level) log-level
              (v:repl-categories) log-repl-categories))
      (fl.util:when-let ((log-debug (find-resource context :log-debug)))
        (ensure-directories-exist log-debug)
        (v:define-pipe ()
          (v:level-filter :level :debug)
          (v:file-faucet :file log-debug)))
      (fl.util:when-let ((log-error (find-resource context :log-error)))
        (ensure-directories-exist log-error)
        (v:define-pipe ()
          (v:level-filter :level :error)
          (v:file-faucet :file log-error))))))
