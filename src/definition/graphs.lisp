(in-package #:%first-light)

(define-graph :core (:category component-dependency)
  (subdag all-unknown-types ((unknown-types)))
  (subdag actions (fl.comp:action -> fl.comp:action-list))
  (subdag drawables (fl.comp:static-mesh -> fl.comp:sprite -> fl.comp:render))
  (subdag core-types
          (fl.comp:transform
           -> (splice actions)
           -> (splice drawables))))

(define-graph :core-component-order (:category component-package-order)
  ;; TODO: if changed to nickname package fl.comp, breaks
  (subdag core-packages (:first-light.components)))
