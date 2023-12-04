(in-package #:virality.texture-map)

(defmacro define-texture-map (name data-model &body body)
  (u:with-gensyms (tmap)
    (destructuring-bind (model style . store)
        (or data-model '(:single :combined))
      `(let ((,tmap
               ,(parse-texture-map name model style (car store) body)))
         (setf (u:href v::=meta/texture-maps= (name ,tmap)) ,tmap)
         ',name))))
