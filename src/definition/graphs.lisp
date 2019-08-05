(in-package #:virality.engine)

(define-graph :core (:category component-dependency)
  (subdag all-unknown-types ((unknown-types)))
  (subdag drawables (comp.mesh.static:static-mesh -> comp.sprite:sprite -> comp.render:render))
  (subdag core-types
          (comp.transform:transform
           -> (splice drawables))))

(define-graph :core-component-order (:category component-package-order)
  (subdag core-packages (:comp)))
