(in-package #:virality.examples)

;;; Materials

(v:define-material graph
  (:profiles (contrib.mat:u-mvpt)
   :shader shd:graph))

(v:define-material 3d-graph
  (:profiles (contrib.mat:u-mvpt)
   :shader shd:3d-graph-1
   :instances 1000
   :attributes (:depth :always)
   :uniforms
   ((:size 1)
    (:min 0)
    (:by 1))))

;;; Prefabs

(v:define-prefab "graph" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("graph" :copy "/mesh")
   (comp.transform:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                            (/ (v:option context :window-height) 2)
                                            0))
   (comp.render:render :material 'graph)))

(v:define-prefab "3d-graph-1" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp.transform:transform :translate (v3:vec 0 70 100))
   (comp.camera:camera (:policy :new-args) :zoom 2)
   (comp.camera.tracking:tracking-camera
    :target-actor (v:ref "/3d-graph-1/graph")))
  (("graph" :copy "/mesh")
   (comp.render:render :material '(3d-graph
                                   3d-graph-1
                                   :shader shd:3d-graph-1
                                   :instances 100000
                                   :uniforms ((:size 0.5))))))

(v:define-prefab "3d-graph-2" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp.transform:transform :translate (v3:vec 0 50 100))
   (comp.camera:camera (:policy :new-args) :zoom 2)
   (comp.camera.tracking:tracking-camera
    :target-actor (v:ref "/3d-graph-2/graph")))
  (("graph" :copy "/mesh")
   (comp.render:render :material '(3d-graph
                                   3d-graph-2
                                   :shader shd:3d-graph-2
                                   :instances 100000
                                   :uniforms ((:size 1))))))

;;; Prefab descriptors

(v:define-prefab-descriptor graph ()
  ("graph" examples))

(v:define-prefab-descriptor 3d-graph-1 ()
  ("3d-graph-1" examples))

(v:define-prefab-descriptor 3d-graph-2 ()
  ("3d-graph-2" examples))
