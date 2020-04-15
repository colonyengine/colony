(in-package #:virality)

(define-graph :core (:category component-dependency)
  (subdag all-unknown-types ((unknown-types)))
  (subdag drawables (comp:static-mesh
                     -> comp:dynamic-mesh
                     -> comp:sprite
                     -> comp:render))
  (subdag core-types (comp:transform -> (splice drawables))))

(define-graph :core-component-order (:category component-package-order)
  (subdag core-packages (:comp)))
