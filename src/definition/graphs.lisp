(in-package #:virality.engine)

(define-graph :core (:category component-dependency)
  (subdag all-unknown-types ((unknown-types)))
  (subdag drawables (c/smesh:static-mesh
                     -> c/dmesh:dynamic-mesh
                     -> c/sprite:sprite
                     -> c/render:render))
  (subdag core-types (c/xform:transform -> (splice drawables))))

(define-graph :core-component-order (:category component-package-order)
  (subdag core-packages (:comp)))
