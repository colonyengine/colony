(in-package :defpackage+-user-1)

(defpackage+ #:first-light.psilord
  (:nicknames #:fl.psilord)
  (:use #:cl
        #:first-light
        #:first-light.components)
  (:export-only #:material-test))

(defpackage+ #:first-light.psilord.shaders
  (:nicknames #:fl.psilord.shaders)
  (:use #:first-light)
  (:inherit #:shadow.vari)
  (:export-only #:test-shader-0))

(defpackage+ #:first-light.psilord.materials
  (:nicknames #:fl.psilord.materials)
  (:use #:cl #:first-light))

(defpackage+ #:first-light.psilord.textures
  (:nicknames #:fl.psilord.textures)
  (:use #:cl #:first-light))
