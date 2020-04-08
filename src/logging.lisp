(in-package #:virality.engine)

(defun enable-logging (core)
  (let ((context (context core)))
    (unless (log:thread log:*global-controller*)
      (log:start log:*global-controller*))
    (when v:=log-repl-enabled=
      (setf (log:repl-level) v:=log-level=
            (log:repl-categories) v:=log-repl-categories=))
    (a:when-let ((log-debug (find-asset context :log-debug)))
      (ensure-directories-exist log-debug)
      (log:define-pipe ()
        (log:level-filter :level :debug)
        (log:file-faucet :file log-debug)))
    (a:when-let ((log-error (find-asset context :log-error)))
      (ensure-directories-exist log-error)
      (log:define-pipe ()
        (log:level-filter :level :error)
        (log:file-faucet :file log-error)))))
