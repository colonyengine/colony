(in-package #:virality.examples)

;;; Materials

(v:define-material graph
  (:profiles (contrib.mat:u-mvpt)
   :shader shaders:graph))

(v:define-material 3d-graph
  (:profiles (contrib.mat:u-mvpt)
   :shader shaders:3d-graph-1
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
   (comp:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                  (/ (v:option context :window-height) 2)
                                  0))
   (comp:render :material 'graph)))

(v:define-prefab "3d-graph-1" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:transform :translate (v3:vec 0 70 100))
   (comp:camera (:policy :new-args) :zoom 2)
   (comp:tracking-camera :target-actor (v:ref "/3d-graph-1/graph")))
  (("graph" :copy "/mesh")
   (comp:render :material '(3d-graph
                            3d-graph-1
                            :shader shaders:3d-graph-1
                            :instances 100000
                            :uniforms ((:size 0.5))))))

(v:define-prefab "3d-graph-2" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:transform :translate (v3:vec 0 50 100))
   (comp:camera (:policy :new-args) :zoom 2)
   (comp:tracking-camera :target-actor (v:ref "/3d-graph-2/graph")))
  (("graph" :copy "/mesh")
   (comp:render :material '(3d-graph
                            3d-graph-2
                            :shader shaders:3d-graph-2
                            :instances 100000
                            :uniforms ((:size 1))))))

;;; Prefab descriptors

(v:define-prefab-descriptor graph ()
  ("graph" examples))

(v:define-prefab-descriptor 3d-graph-1 ()
  ("3d-graph-1" examples))

(v:define-prefab-descriptor 3d-graph-2 ()
  ("3d-graph-2" examples))
