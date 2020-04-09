(in-package #:virality)

(defmacro with-continuable (report &body body)
  `(restart-case (progn ,@body)
     (continue () :report ,report)))

(defun compile-repl-functions ()
  (let ((repl-package (find-if #'find-package '(:slynk :swank))))
    (macrolet ((sym (sym &optional package)
                 (let ((name (symbol-name sym)))
                   `(a:ensure-symbol ,name ,(or package 'repl-package)))))
      (case repl-package
        ((:slynk :swank)
         (compile '%find-repl
                  `(lambda ()
                     (or ,(sym :*emacs-connection*)
                         (,(sym :default-connection)))))
         (compile '%setup-repl
                  (ecase repl-package
                    (:slynk
                     `(lambda ()
                        (a:when-let ((repl (find
                                            (,(sym :current-thread))
                                            (,(sym :channels))
                                            :key #',(sym :channel-thread))))
                          (,(sym :send-prompt :slynk-mrepl) repl))))
                    (:swank
                     (constantly nil))))
         (compile '%update-repl
                  `(lambda ()
                     (a:when-let ((repl (%find-repl)))
                       (with-continuable "REPL"
                         (,(sym :handle-requests) repl t))))))
        (t (setf (symbol-function '%setup-repl) (constantly nil)
                 (symbol-function '%update-repl) (constantly nil)))))))

(defun setup-repl ()
  (compile-repl-functions)
  (funcall '%setup-repl))

(defun update-repl ()
  (funcall '%update-repl))
