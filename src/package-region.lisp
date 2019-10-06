(in-package #:cl-user)

;; NOTE: The API is still somewhat fluid.
(defpackage #:virality.region
  (:use #:cl)
  (:export

   ;; Base region class type
   #:region
   #:center

   ;; cuboid API
   #:region-cuboid
   #:minx
   #:maxx
   #:miny
   #:maxy
   #:minz
   #:maxz
   ;; sphere API
   #:make-region-cuboid
   #:region-sphere
   #:radius
   #:make-region-sphere

   ;; ellipsoid API
   #:region-ellipsoid
   #:x
   #:y
   #:z
   #:make-region-ellipsoid

   ;; utilities dealing with regions
   #:clip-movement-vector))
