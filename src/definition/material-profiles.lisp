(in-package #:virality.extension)

(v:define-material-profile u-model
  (:uniforms
   ((:model (m4:mat 1)))))

(v:define-material-profile u-view
  (:uniforms
   ((:view (m4:mat 1)))))

(v:define-material-profile u-proj
  (:uniforms
   ((:proj (m4:mat 1)))))

(v:define-material-profile u-time
  (:uniforms
   ((:time #'v:total-time))))

(v:define-material-profile u-mvp
  (:uniforms
   ((:model (m4:mat 1))
    (:view (m4:mat 1))
    (:proj (m4:mat 1)))))

(v:define-material-profile u-vp
  (:uniforms
   ((:view (m4:mat 1))
    (:proj (m4:mat 1)))))

(v:define-material-profile u-mvpt
  (:uniforms
   ((:model (m4:mat 1))
    (:view (m4:mat 1))
    (:proj (m4:mat 1))
    (:time #'v:total-time))))

(v:define-material-profile u-mvptr
  (:uniforms
   ((:model (m4:mat 1))
    (:view (m4:mat 1))
    (:proj (m4:mat 1))
    (:time #'v:total-time)
    (:res #'v:screen-resolution))))

(v:define-material-profile u-vpt
  (:uniforms
   ((:view (m4:mat 1))
    (:proj (m4:mat 1))
    (:time #'v:total-time))))
