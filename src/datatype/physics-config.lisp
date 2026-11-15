(in-package #:colony)

(defclass physics-config ()
  (;; A list of symbols indicates the legal collision layers.
   (%physics-layers :reader physics-layers
                    :initarg :physics-layers
                    :initform nil)
   ;; A hash table that indicate what layer collides with what other layers.
   (%collision-plan :reader collision-plan
                    :initarg :collision-plan
                    ;; KEY: layer symbol
                    ;; VALUE: list of layers the KEY collides with
                    :initform (u:dict #'eq))))
