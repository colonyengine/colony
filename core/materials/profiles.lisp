(in-package :first-light.materials)

(fl:define-material-profile u-model
  (:uniforms
   ((:model (flm:mat4 1)))))

(fl:define-material-profile u-view
  (:uniforms
   ((:view (flm:mat4 1)))))

(fl:define-material-profile u-proj
  (:uniforms
   ((:proj (flm:mat4 1)))))

(fl:define-material-profile u-time
  (:uniforms
   ((:time #'total-time))))

(fl:define-material-profile u-mvp
  (:uniforms
   ((:model (flm:mat4 1))
    (:view (flm:mat4 1))
    (:proj (flm:mat4 1)))))

(fl:define-material-profile u-vp
  (:uniforms
   ((:view (flm:mat4 1))
    (:proj (flm:mat4 1)))))

(fl:define-material-profile u-mvpt
  (:uniforms
   ((:model (flm:mat4 1))
    (:view (flm:mat4 1))
    (:proj (flm:mat4 1))
    (:time #'total-time))))

(fl:define-material-profile u-vpt
  (:uniforms
   ((:view (flm:mat4 1))
    (:proj (flm:mat4 1))
    (:time #'total-time))))
