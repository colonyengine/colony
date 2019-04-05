(in-package :%first-light)

(defmacro with-continue-restart (report &body body)
  `(restart-case (progn ,@body)
     (continue () :report ,report)))

#+(or slynk swank)
(macrolet ((install-repl-support ()
             (let ((repl-package (car (intersection '(:swank :slynk)
                                                    *features*))))
               `(progn
                  (defun find-lisp-repl ()
                    (when ,repl-package
                      (load-time-value
                       (or ,(au:ensure-symbol "*EMACS-CONNECTION*" repl-package)
                           (,(au:ensure-symbol "DEFAULT-CONNECTION"
                                               repl-package))))))
                  (defun setup-lisp-repl ()
                    (if ,(eq repl-package :slynk)
                        (,(au:ensure-symbol "SEND-PROMPT" "SLYNK-MREPL")
                         (find (bt:current-thread)
                               (,(au:ensure-symbol "CHANNELS" repl-package))
                               :key #',(au:ensure-symbol "CHANNEL-THREAD"
                                                         repl-package)))
                        (au:noop)))
                  (defun update-lisp-repl ()
                    (if ,repl-package
                        (au:when-let ((repl (find-lisp-repl)))
                          (with-continue-restart "REPL"
                            (,(au:ensure-symbol "HANDLE-REQUESTS" repl-package)
                             repl t)))
                        (au:noop)))))))
  (install-repl-support))

#-(or slynk swank)
(progn
  (defun setup-lisp-repl ()
    (au:noop))
  (defun update-lisp-repl ()
    (au:noop)))
