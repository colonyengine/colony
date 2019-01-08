(in-package :first-light.components)

(define-component action-list ()
  ((actions :default (fl.dst:make-dlist :test #'eq))))

(defmethod on-component-update ((self action-list))
  (with-accessors ((actions actions)) self
    (loop :for (name . action) :in (fl.dst:dlist-elements actions)
          :do (on-action-update action name)
          :when (finished-p action)
            :do (on-action-finish action name)
          :when (blocking-p action)
            :do (return))))
