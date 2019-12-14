(in-package #:virality.examples)

;;; Materials

(v:define-material graph
  (:profiles (x/mat:u-mvpt)
   :shader ex/shd:graph))

(v:define-material 3d-graph
  (:profiles (x/mat:u-mvpt)
   :shader ex/shd:3d-graph-1
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
   (c/xform:transform :scale (v3:vec (/ (v:option context :window-width) 2f0)
                                     (/ (v:option context :window-height) 2f0)
                                     0f0))
   (c/render:render :material 'graph)))

(v:define-prefab "3d-graph-1" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (c/xform:transform :translate (v3:vec 0f0 70f0 100f0))
   (c/cam:camera (:policy :new-args) :zoom 2f0)
   (c/tcam:tracking-camera :target-actor (v:ref "/3d-graph-1/graph")))
  (("graph" :copy "/mesh")
   (c/render:render :material '(3d-graph
                                3d-graph-1
                                :shader ex/shd:3d-graph-1
                                :instances 100000
                                :uniforms ((:size 0.5f0))))))

(v:define-prefab "3d-graph-2" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (c/xform:transform :translate (v3:vec 0f0 50f0 100f0))
   (c/cam:camera (:policy :new-args) :zoom 2f0)
   (c/tcam:tracking-camera
    :target-actor (v:ref "/3d-graph-2/graph")))
  (("graph" :copy "/mesh")
   (c/render:render :material '(3d-graph
                                3d-graph-2
                                :shader ex/shd:3d-graph-2
                                :instances 100000
                                :uniforms ((:size 1))))))

;;; Prefab descriptors

(v:define-prefab-descriptor graph ()
  ("graph" examples))

(v:define-prefab-descriptor 3d-graph-1 ()
  ("3d-graph-1" examples))

(v:define-prefab-descriptor 3d-graph-2 ()
  ("3d-graph-2" examples))
