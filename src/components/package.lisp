(in-package :defpackage+-1)

(defpackage+ #:fl.comp.stub-0
  (:inherit #:fl.core)
  (:export-only #:stub-0
                #:value))

(defpackage+ #:fl.comp.transform
  (:inherit #:fl.core)
  (:export-only #:transform
                #:model
                #:local
                #:add-child
                #:map-nodes))

(defpackage+ #:fl.comp.camera
  (:inherit #:fl.core
            #:fl.comp.transform)
  (:export-only #:camera
                #:transform
                #:view
                #:projection
                #:zoom-camera
                #:compute-camera-view))

(defpackage+ #:fl.comp.following-camera
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera)
  (:export-only #:following-camera))

(defpackage+ #:fl.comp.tracking-camera
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera)
  (:export-only #:tracking-camera))

(defpackage+ #:fl.comp.mesh
  (:inherit #:fl.core)
  (:export-only #:mesh
                #:write-buffer-data
                #:update-mesh-buffer
                #:make-vao
                #:vao
                #:load-mesh))

(defpackage+ #:fl.comp.mesh-renderer
  (:inherit #:fl.core
            #:fl.comp.transform
            #:fl.comp.camera
            #:fl.comp.mesh)
  (:export-only #:mesh-renderer))

(defpackage+ #:fl.comp.tags
  (:inherit #:fl.core)
  (:export-only #:tags))
