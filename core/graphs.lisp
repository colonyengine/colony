(in-package :%first-light)

(define-graph :core (:category :component-dependency)
  (subdag all-unknown-types ((unknown-types)))
  (subdag meshes (fl.comp:mesh -> fl.comp:mesh-renderer))
  (subdag core-types (fl.comp:transform (splice meshes))))

(define-graph :core-component-order (:category :component-package-order)
  ;; TODO: if changed to nickname package fl.comp, breaks
  (subdag core-packages (:first-light.components)))
