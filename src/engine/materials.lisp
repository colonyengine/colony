(in-package :fl.core)

(defclass material ()
  ((id :reader id
       :initarg :id)
   (shader :reader shader
           :initarg :shader
           :initform ::default)
   (factors :reader factors
            :initarg :factors)
   (maps :reader maps
         :initarg :maps)))

(defclass material-factors ()
  ((diffuse :reader diffuse
            :initarg :diffuse
            :initform (vec4 1 1 1 1))
   (metalness :reader metalness
              :initarg :metalness
              :initform 1.0)
   (roughness :reader roughness
              :initarg :roughness
              :initform 1.0)
   (emissive :reader emissive
             :initarg :emissive
             :initform (v3zero))))

(defclass material-maps ()
  ((diffuse :reader diffuse
            :initarg :diffuse
            :initform nil)
   (normal :reader normal
           :initarg :normal
           :initform nil)
   (metalness/roughness/occlusion :reader metalness/roughness/occlusion
                                  :initarg :metalness/roughness/occlusion
                                  :initform nil)
   (emissive :reader emmissive
             :initarg :emissive
             :initform nil)))
