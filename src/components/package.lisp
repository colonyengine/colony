(in-package :defpackage+-1)

(defpackage+ #:fl.comp
  (:use #:cl)
  (:inherit #:fl.core)
  (:export-only #:define-shared-storage
                #:with-shared-storage))

(defpackage+ #:fl.comp.transform
  (:use #:cl)
  (:inherit #:fl.core
            #:fl.comp)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat))
  (:export-only #:transform
                #:model
                #:local
                #:add-child
                #:remove-child
                #:map-nodes))

(defpackage+ #:fl.comp.camera
  (:use #:cl)
  (:inherit #:fl.core
            #:fl.comp
            #:fl.comp.transform)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat))
  (:export-only #:camera
                #:transform
                #:view
                #:projection
                #:zoom-camera
                #:compute-camera-view))

(defpackage+ #:fl.comp.following-camera
  (:use #:cl)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat))
  (:inherit #:fl.core
            #:fl.comp
            #:fl.comp.transform
            #:fl.comp.camera)
  (:export-only #:following-camera))

(defpackage+ #:fl.comp.tracking-camera
  (:use #:cl)
  (:local-nicknames (#:v3 #:box.math.vec3)
                    (#:m4 #:box.math.mat4)
                    (#:q #:box.math.quat))
  (:inherit #:fl.core
            #:fl.comp
            #:fl.comp.transform
            #:fl.comp.camera)
  (:export-only #:tracking-camera))

(defpackage+ #:fl.comp.mesh
  (:use #:cl)
  (:inherit #:fl.core
            #:fl.comp)
  (:export-only #:mesh
                #:primitives))

(defpackage+ #:fl.comp.mesh-renderer
  (:use #:cl)
  (:inherit #:fl.core
            #:fl.comp
            #:fl.comp.transform
            #:fl.comp.camera
            #:fl.comp.mesh)
  (:export-only #:mesh-renderer))

(defpackage+ #:fl.comp.tags
  (:use #:cl)
  (:inherit #:fl.core
            #:fl.comp)
  (:export-only #:tags))
