;; -*- mode: common-lisp -*-

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

;;; vertex layouts

(define-vertex-layout 2d
  (:groups (2d)))

(define-vertex-layout 2d/color
  (:groups (2d color)))

(define-vertex-layout 3d/normal
  (:groups (3d normal)))

(define-vertex-layout 3d/normal/color
  (:groups (3d normal color)))

;;; Test groups/layouts
;;; These two definitions are to test how an object with more than 1 VBO behaves.
;;; The test.mesh mesh file uses the 2d/color-test layout.

(define-vertex-groups color-test
  ((:id :color-test
    :attrs ((color :float 4)))))

(define-vertex-layout 2d/color-test
  (:groups (2d color-test)))
