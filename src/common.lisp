(in-package #:virality.engine)

(defvar *core-debug*)

(defun type-table (key type-table)
  (u:href type-table key))

(defun (setf type-table) (entry type-name-key type-table)
  (symbol-macrolet ((entry-table (u:href type-table type-name-key)))
    (unless (nth-value 1 entry-table)
      (setf entry-table (u:dict)))
    (setf (u:href entry-table entry) entry)))

(defun type-table-drop (component component-type type-table)
  (remhash component (type-table component-type type-table)))

(defun recompile-queued-items (core)
  (loop :for ((kind data) found-p) = (multiple-value-list
                                      (pop-queue :live-recompile))
        :while found-p
        :do (ecase kind
              (:shader
               (gpu:recompile-shaders data))
              ((:texture :material)
               (funcall data core)))))
