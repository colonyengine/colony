(asdf:defsystem #:first-light.example
  :description "Examples for first-light."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:first-light)
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:module "shaders"
    :components
    ((:file "damaged-helmet")
     (:file "graph")
     (:file "noise")
     (:file "starfield")
     (:file "texture")))
   (:file "example-collision")
   (:file "example-damaged-helmet")
   (:file "example-geometric-volumes")
   (:file "example-graph")
   (:file "example-isometric-view")
   (:file "example-noise")
   (:file "example-sprite")
   (:file "example-texture")
   ;; TODO: Disabled until PTP branch is merged in. This is because the
   ;; define-component syntax changed and will reduce merge conflicts (:file
   ;; "example-lisp-game-jam-april-2019")
   ))

;; broken: isometric-view
