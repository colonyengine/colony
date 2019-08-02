(in-package #:%first-light)

(defgeneric on-component-initialize (component)
  (:method ((self component))))

(defgeneric on-component-attach (component actor)
  (:method ((self component) actor)))

(defgeneric on-component-detach (component actor)
  (:method ((self component) actor)))

(defgeneric on-component-physics-update (component)
  (:method ((self component))))

(defgeneric on-component-update (component)
  (:method ((self component))))

(defgeneric on-component-render (component)
  (:method ((self component))))

(defgeneric on-component-destroy (component)
  (:method ((self component))))
