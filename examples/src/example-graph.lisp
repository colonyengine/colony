(in-package #:colony-examples)

;;; Materials

(c:define-material graph
  (:profiles (x:u-mvpt)
   :shader ex/shd:graph))

(c:define-material 3d-graph
  (:profiles (x:u-mvpt)
   :shader ex/shd:3d-graph-1
   :instances 1000
   :attributes (:depth :always)
   :uniforms
   ((:size 1)
    (:min 0)
    (:by 1))))

;;; Prefabs

(c:define-prefab "graph" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  (("graph" :copy "/mesh")
   (comp:transform :scale (v3:vec (/ c:=window-width= 2f0)
                                  (/ c:=window-height= 2f0)
				  0f0))
   (comp:render :material 'graph
                :slave (c:ref :self :component 'comp:mesh))))

(c:define-prefab "3d-graph-1" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:transform :translate (v3:vec 0f0 70f0 100f0))
   (comp:camera (:policy :new-args) :zoom 2f0)
   (comp:tracking-camera :target-actor (c:ref "/3d-graph-1/graph")))
  (("graph" :copy "/mesh")
   (comp:render :material '(3d-graph
                            3d-graph-1
                            :shader ex/shd:3d-graph-1
                            :instances 100000
                            :uniforms ((:size 0.5f0)))
                :slave (c:ref :self :component 'comp:mesh))))

(c:define-prefab "3d-graph-2" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:transform :translate (v3:vec 0f0 50f0 100f0))
   (comp:camera (:policy :new-args) :zoom 2f0)
   (comp:tracking-camera
    :target-actor (c:ref "/3d-graph-2/graph")))
  (("graph" :copy "/mesh")
   (comp:render :material '(3d-graph
                            3d-graph-2
                            :shader ex/shd:3d-graph-2
                            :instances 100000
                            :uniforms ((:size 1)))
                :slave (c:ref :self :component 'comp:mesh))))
