(in-package #:colony)

(defclass physics-config ()
  (;; We track the name of this physics-config for easier debugging
   (%name :reader name
          :initarg :name
          :initform nil)
   ;; A list of symbols indicates the legal collision layers.
   (%physics-layers :reader physics-layers
                    :initarg :physics-layers
                    :initform nil)
   ;; A hash table that indicate what layer collides with what other layers.
   (%collision-plan :reader collision-plan
                    :initarg :collision-plan
                    ;; KEY: layer symbol
                    ;; VALUE: list of layers the KEY collides with
                    :initform (u:dict #'eq))))

;; Used in the meta-space to represent a specific physics-config
(defclass physics-config-descriptor ()
  ((%name :reader name
          :initarg :name
          :initform nil)
   (%original-form :reader original-form
                   :initarg :original-form
                   :initform nil)
   (%constructor-form :reader constructor-form
                      :initarg :constructor-form
                      :initform nil)
   (%constructor :reader constructor
                 :initarg :constructor
                 :initform nil)))
