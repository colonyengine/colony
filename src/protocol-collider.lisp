(in-package #:%first-light)

(defgeneric on-collision-enter (component other-collider)
  (:method ((self component) other-collider)))

(defgeneric on-collision-continue (component other-collider)
  (:method ((self component) other-collider)))

(defgeneric on-collision-exit (component other-collider)
  (:method ((self component) other-collider)))
