(in-package #:virality.engine)

(define-graph :core (:category component-dependency)
  (subdag all-unknown-types ((unknown-types)))
  (subdag actions (comp:action -> comp:action-list))
  (subdag drawables (comp:static-mesh -> comp:sprite -> comp:render))
  (subdag core-types
          (comp:transform
           -> (splice actions)
           -> (splice drawables))))

(define-graph :core-component-order (:category component-package-order)
  (subdag core-packages (:comp)))
