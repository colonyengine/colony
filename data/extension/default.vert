;; -*- mode: common-lisp -*-

;;; vertex groups

(define-vertex-groups vertex
  ((:id :vertex
    :attrs ((position :float 3)
            (normal :float 3)
            (tangent :float 4)
            (color :float 4)))))

(define-vertex-groups texture
  ((:id :texture
    :attrs ((uv1 :float 2)
            (uv2 :float 2)))))

(define-vertex-groups bones
  ((:id :bones
    :attrs ((joints :unsigned-short 4)
            (weights :float 4)))))

;;; vertex layouts

(define-vertex-layout default-mesh
  (:groups (vertex texture bones)))
