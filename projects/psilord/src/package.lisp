(in-package :defpackage+-user-1)

(defpackage+ #:first-light.psilord
  (:nicknames #:fl.psilord)
  (:use #:cl
        #:first-light
        #:first-light.components))

(defpackage+ #:first-light.psilord.materials
  (:nicknames #:fl.psilord.materials)
  (:use #:cl #:first-light))

(defpackage+ #:first-light.psilord.textures
  (:nicknames #:fl.psilord.textures)
  (:use #:cl #:first-light))
