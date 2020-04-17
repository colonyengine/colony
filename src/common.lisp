(in-package #:virality)

(defun recompile-queued-items (core)
  (loop :for ((kind data) found-p) = (multiple-value-list
                                      (pop-queue :recompile))
        :while found-p
        :do (ecase kind
              (:shader
               (shadow:recompile-shaders data))
              ((:texture :material)
               (funcall data core)))))
