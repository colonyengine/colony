(in-package #:virality)

(defmacro with-continuable (report &body body)
  `(restart-case (progn ,@body)
     (continue () :report ,report)))

(defun setup-repl ()
  (case (find-if #'find-package '(:slynk :swank))
    (:slynk
     (funcall (find-symbol "SEND-PROMPT" :slynk-mrepl))
     (compile
      'update-repl
      `(lambda ()
         (with-continuable "REPL"
           (,(find-symbol "PROCESS-REQUESTS" :slynk) t)))))
    (:swank
     (compile
      'update-repl
      `(lambda ()
         (u:when-let ((repl (or (find-symbol "*EMACS-CONNECTION*" :swank)
                                (find-symbol "DEFAULT-CONNECTION" :swank))))
           (with-continuable "REPL"
             (,(find-symbol "HANDLE-REQUESTS" :swank) repl t))))))
    (t (setf (symbol-function 'update-repl) (constantly nil)))))

(defun recompile-queued-items (core)
  (loop :for ((kind data) found-p) = (multiple-value-list
                                      (pop-queue :recompile))
        :while found-p
        :do (ecase kind
              (:shader
               (shadow:recompile-shaders data))
              ((:texture :material :prefab)
               (funcall data core)))))
