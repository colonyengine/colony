(in-package #:colony)

(c:define-physics-config default-physics ()
  (c:physics-layers
   (c:definitions
    (A :default))
   (c:collision-plan
    (c:table-spec
     ;; rows and columns are REQUIRED to be in same order.
     ((  A)
      (A y))))))
