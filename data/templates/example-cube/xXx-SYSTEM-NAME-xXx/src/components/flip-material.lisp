(in-package #:xXx-SYSTEM-NAME-xXx)

(defmethod c:on-component-initialize ((self flip-material))
  (with-accessors ((renderer renderer)
                   (flip-hz flip-hz))
      self
    (when (<= flip-hz 0f0)
      (setf flip-hz 1f0))))

(defmethod c:on-component-update ((self flip-material))
  (with-accessors ((context c:context)
                   (renderer renderer)
                   (material-array material-array)
                   (curr-idx curr-idx)
                   (flip-hz flip-hz)
                   (time-accumulated time-accumulated))
      self

    (let ((previous-idx curr-idx)
          (flip-delta (/ 1f0 flip-hz)))

      (when (>= time-accumulated flip-delta)
        (decf time-accumulated flip-delta)
        (incf curr-idx))

      (when (/= previous-idx curr-idx)
        (setf curr-idx (mod curr-idx (length material-array))
              (comp:material renderer) (aref material-array curr-idx)))

      (incf time-accumulated (c:frame-time context)))))
