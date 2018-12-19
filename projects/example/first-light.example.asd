(asdf:defsystem #:first-light.example
  :description "Example scene for first-light."
  :author ("Michael Fiano <michael.fiano@gmail.com>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <michael.fiano@gmail.com>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:first-light)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:module "components"
    :components
    ((:file "shader-sweep")))

   (:module "examples"
    :components

    ((:module "damaged-helmet"
      :components
      ((:file "shaders")
       (:file "textures")
       (:file "materials")
       (:file "scene")))

     (:module "geometric-volumes"
      :components
      ((:file "scene")))

     (:module "graph-test"
      :components
      ((:file "shaders")
       (:file "materials")
       (:file "scene")))

     (:module "isometric-view-test"
      :components
      ((:file "scene")))

     (:module "noise-test"
      :components
      ((:file "shaders")
       (:file "materials")
       (:file "scene")))

     (:module "sprite-test"
      :components
      ((:file "shaders")
       (:file "textures")
       (:file "materials")
       (:file "scene")
       (:file "example")))

     (:module "texture-test"
      :components
      ((:file "shaders")
       (:file "textures")
       (:file "materials")
       (:file "scene")))))))
