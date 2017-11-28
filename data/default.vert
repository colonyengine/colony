;;; vertex groups

(define-vertex-groups 2d
  ((:attrs ((pos :float 2)
            (uv :float 2)))))

(define-vertex-groups 3d
  ((:attrs ((pos :float 3)
            (uv :float 3)))))

(define-vertex-groups color
  ((:attrs ((color :float 4)))))

(define-vertex-groups normal
  ((:attrs ((normal :float 3)))))

(define-vertex-groups instance-test
  ((:attrs ((tangent :float 3)))
   (:id :test-data
    :divisor 1
    :attrs ((test-a :float 2)
            (test-b :float 3)))))

;;; vertex layouts

(define-vertex-layout mesh-layout
  (:groups (3d normal color instance-test)))

;;; meshes

(define-mesh example-mesh (mesh-layout)
  ((:mesh-data
    (((0.5 0.5 0) (1 1 0) (0 0 1) (1 0 0 1) (0 0 0))
     ((-0.5 0.5 0) (0 1 0) (0 0 1) (0 1 0 1) (0 0 0))
     ((-0.5 -0.5 0) (0 0 0) (0 0 1) (0 0 1 1) (0 0 0))
     ((0.5 0.5 0) (1 1 0) (0 0 1) (1 0 0 1) (0 0 0))
     ((-0.5 -0.5 0) (0 0 0) (0 0 1) (0 0 1 1) (0 0 0))
     ((0.5 -0.5 0) (1 0 0) (0 0 1) (0 1 0 1) (0 0 0))))
   (:test-data
    (((0.1 0.2) (0.1 0.2 0.3))
     ((0.2 0.4) (0.2 0.4 0.6))
     ((0 0) (0 0 0))
     ((0 1) (0 1 2))
     ((1 2) (1 2 3))
     ((0 1) (2 3 4))))))
