(in-package #:first-light.example)

;;; Materials

(fl:define-material graph
  (:profiles (fl.materials:u-mvpt)
   :shader fl.shader.user:graph))

(fl:define-material 3d-graph
  (:profiles (fl.materials:u-mvpt)
   :shader fl.shader.user:3d-graph-1
   :instances 1000
   :attributes (:depth :always)
   :uniforms
   ((:size 1)
    (:min 0)
    (:by 1))))

;;; Prefabs

(fl:define-prefab "graph" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("graph" :copy "/mesh")
   (fl.comp:transform :scale (v3:make (/ (fl:option context :window-width) 2)
                                      (/ (fl:option context :window-height) 2)
                                      0))
   (fl.comp:render :material 'graph)))

(fl:define-prefab "3d-graph-1" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (fl.comp:transform :translate (v3:make 0 70 100))
   (fl.comp:camera (:policy :new-args) :zoom 2)
   (fl.comp:tracking-camera :target-actor (fl:ref "/3d-graph-1/graph")))
  (("graph" :copy "/mesh")
   (fl.comp:render :material '(3d-graph
                               3d-graph-1
                               :shader fl.shader.user:3d-graph-1
                               :instances 100000
                               :uniforms ((:size 0.5))))))

(fl:define-prefab "3d-graph-2" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (fl.comp:transform :translate (v3:make 0 50 100))
   (fl.comp:camera (:policy :new-args) :zoom 2)
   (fl.comp:tracking-camera :target-actor (fl:ref "/3d-graph-2/graph")))
  (("graph" :copy "/mesh")
   (fl.comp:render :material '(3d-graph
                               3d-graph-2
                               :shader fl.shader.user:3d-graph-2
                               :instances 100000
                               :uniforms ((:size 1))))))

;;; Prefab descriptors

(fl:define-prefab-descriptor graph ()
  ("graph" fl.example:examples))

(fl:define-prefab-descriptor 3d-graph-1 ()
  ("3d-graph-1" fl.example:examples))

(fl:define-prefab-descriptor 3d-graph-2 ()
  ("3d-graph-2" fl.example:examples))
