(in-package #:cl-user)

(defpackage #:virality.test
  (:local-nicknames
   (#:u #:vutils)
   (#:const #:vorigin.constants)
   (#:m2 #:vorigin.mat2)
   (#:m3 #:vorigin.mat3)
   (#:m4 #:vorigin.mat4)
   (#:q #:vorigin.quat)
   (#:v2 #:vorigin.vec2)
   (#:v3 #:vorigin.vec3)
   (#:v4 #:vorigin.vec4)
   (#:abag #:virality.attribute-bag)
   (#:clone #:virality.clone))
  (:use
   #:cl
   #:parachute))
