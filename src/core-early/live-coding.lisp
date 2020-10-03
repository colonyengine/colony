(in-package #:virality)

(defmacro with-continuable (report &body body)
  (u:with-gensyms (debugger-entry-time previous-hook pause-time)
    (let ((hook #+sbcl 'sb-ext:*invoke-debugger-hook*
                #-sbcl '*debugger-hook*)
          ;; TODO: Deal with when somehow *core-debug* is unbound.
          (clock '(clock *core-debug*)))
      `(let* ((,debugger-entry-time nil)
              (,previous-hook ,hook)
              (,hook
                (lambda (condition hook)
                  (declare (ignore hook))
                  (format t "Entered debugger from ~a~%" ,report)
                  (setf ,debugger-entry-time (get-time ,clock))
                  (when ,previous-hook
                    (funcall ,previous-hook condition ,previous-hook)))))
         (restart-case (progn ,@body)
           (continue ()
             :report ,report
             (when ,debugger-entry-time
               (let ((,pause-time (- (get-time ,clock) ,debugger-entry-time)))
                 (incf (pause-time ,clock) ,pause-time)
                 (format t "Spent ~d seconds in the debugger."
                         ,pause-time)))))))))

(flet ((generate-live-support-functions ()
         (let ((repl-package (find-if #'find-package '(:slynk :swank))))
           (compile
            'setup-repl
            (if (eq repl-package :slynk)
                `(lambda ()
                   (,(find-symbol "SEND-PROMPT" :slynk-mrepl)))
                (constantly nil)))
           (compile
            'update-repl
            (case repl-package
              (:slynk
               `(lambda ()
                  (with-continuable "REPL"
                    (,(find-symbol "PROCESS-REQUESTS" :slynk) t))))
              (:swank
               `(lambda ()
                  (u:when-let ((repl (or ,(find-symbol "*EMACS-CONNECTION*"
                                                       :swank)
                                         (,(find-symbol "DEFAULT-CONNECTION"
                                                        :swank)))))
                    (with-continuable "REPL"
                      (,(find-symbol "HANDLE-REQUESTS" :swank)
                       repl t)))))
              (t (constantly nil))))
           (compile
            'send-to-repl
            (if (eq repl-package :slynk)
                (constantly nil)
                ;; TODO: uncomment and remove constantly line above when Sly
                ;; merges in send-to-repl to master.
                #++`(lambda (values &key (comment "Sent from Pyx"))
                   (,(find-symbol "COPY-TO-REPL-IN-EMACS" :slynk-mrepl)
                    values :blurb comment :pop-to-buffer nil))
                (constantly nil))))))
  (generate-live-support-functions))

(defun recompile-queued-items (core)
  (loop :for ((kind data) found-p) = (multiple-value-list
                                      (pop-queue :recompile))
        :while found-p
        :do (ecase kind
              (:shader
               (shadow:recompile-shaders data))
              ((:texture :material :prefab)
               (funcall data core)))))
