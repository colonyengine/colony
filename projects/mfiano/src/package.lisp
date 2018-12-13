(in-package :defpackage+-user-1)

(defpackage+ #:first-light.mfiano
  (:nicknames #:fl.mfiano)
  (:use #:cl #:first-light)
  (:export-only #:noise-test/playground
                #:mesh-test))

(defpackage+ #:first-light.mfiano.shaders
  (:nicknames #:fl.mfiano.shaders)
  (:use #:first-light)
  (:inherit #:shadow.vari)
  (:export-only #:noise-test/playground
                #:mesh-test))

(defpackage+ #:first-light.mfiano.materials
  (:nicknames #:fl.mfiano.materials)
  (:use #:cl #:first-light.materials)
  (:import-from #:first-light #:define-material))

(defpackage+ #:first-light.mfiano.textures
  (:nicknames #:fl.mfiano.textures)
  (:use #:cl #:first-light))
