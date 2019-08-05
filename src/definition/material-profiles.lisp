(in-package #:virality.contrib.materials)

(mat:define-material-profile u-model
  (:uniforms
   ((:model (m4:id)))))

(mat:define-material-profile u-view
  (:uniforms
   ((:view (m4:id)))))

(mat:define-material-profile u-proj
  (:uniforms
   ((:proj (m4:id)))))

(mat:define-material-profile u-time
  (:uniforms
   ((:time #'v:total-time))))

(mat:define-material-profile u-mvp
  (:uniforms
   ((:model (m4:id))
    (:view (m4:id))
    (:proj (m4:id)))))

(mat:define-material-profile u-vp
  (:uniforms
   ((:view (m4:id))
    (:proj (m4:id)))))

(mat:define-material-profile u-mvpt
  (:uniforms
   ((:model (m4:id))
    (:view (m4:id))
    (:proj (m4:id))
    (:time #'v:total-time))))

(mat:define-material-profile u-vpt
  (:uniforms
   ((:view (m4:id))
    (:proj (m4:id))
    (:time #'v:total-time))))
