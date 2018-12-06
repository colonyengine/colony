(in-package :fl.example)

(define-resources (:project :fl.example)
  (:project "data")
  (:mesh (:project "mesh"))
  (:texture (:project "texture"))

  ;; damaged helmet
  (:damaged-helmet-textures (:project :texture "damaged-helmet/")))
