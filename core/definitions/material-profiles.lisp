(in-package :first-light.materials)

(fl:define-material-profile u-model
  (:uniforms
   ((:model (m:mat4 1)))))

(fl:define-material-profile u-view
  (:uniforms
   ((:view (m:mat4 1)))))

(fl:define-material-profile u-proj
  (:uniforms
   ((:proj (m:mat4 1)))))

(fl:define-material-profile u-time
  (:uniforms
   ((:time #'total-time))))

(fl:define-material-profile u-mvp
  (:uniforms
   ((:model (m:mat4 1))
    (:view (m:mat4 1))
    (:proj (m:mat4 1)))))

(fl:define-material-profile u-vp
  (:uniforms
   ((:view (m:mat4 1))
    (:proj (m:mat4 1)))))

(fl:define-material-profile u-mvpt
  (:uniforms
   ((:model (m:mat4 1))
    (:view (m:mat4 1))
    (:proj (m:mat4 1))
    (:time #'total-time))))

(fl:define-material-profile u-vpt
  (:uniforms
   ((:view (m:mat4 1))
    (:proj (m:mat4 1))
    (:time #'total-time))))
