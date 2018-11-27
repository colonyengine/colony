(in-package #:fl.example)

(defmethod prologue (context)
  (format t "Running PROLOGUE method!~%")

  ;; Example 0: Unrealized Procedural Textures.
  ;; These are textures whose texture descriptors are :procedural and
  ;; also referenced in a define-material.
  (format T "Found unrealized textures: ~A~%"
          (%fl::get-unrealized-textures context))
  )
