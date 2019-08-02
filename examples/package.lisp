(in-package #:cl-user)

(defpackage #:first-light.example
  (:nicknames #:fl.example)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:log #:verbose)
                    (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m4 #:origin.mat4)
                    (#:q #:origin.quat)
                    (#:v #:first-light)
                    (#:comp #:first-light.components)
                    (#:contrib.tex #:virality.contrib.textures)
                    (#:contrib.mat #:virality.contrib.materials)
                    (#:contrib.action #:virality.contrib.actions))
  (:use #:cl)
  (:export #:examples
           #:collision-smoke-test
           #:collision-test-0
           #:collision-test-1
           #:damaged-helmet
           #:geometric-volumes
           #:graph
           #:3d-graph-1
           #:3d-graph-2
           #:isometric-view
           #:noise
           #:sprite-1
           #:sprite-2
           #:texture))

;;;; This is a reasonably scoped shmup that demonstrates use of the engine.
(defpackage #:first-light.examples.protect-the-planets
  (:nicknames #:fl.examples.ptp)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:log #:verbose)
                    (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m4 #:origin.mat4)
                    (#:q #:origin.quat)
                    (#:v #:first-light)
                    (#:comp #:first-light.components)
                    (#:contrib.tex #:virality.contrib.textures)
                    (#:contrib.mat #:virality.contrib.materials)
                    (#:contrib.action #:virality.contrib.actions))
  (:use #:cl)
  (:export #:ptp-base
           #:ptp
           #:starfield-demo))
