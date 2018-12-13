(in-package :first-light.example)

(define-resources (:project :first-light.example)
  (:project "data/project")
  (:ext (:project "ext"))
  (:mesh (:project "mesh"))
  (:texture (:project "texture"))
  (:log (:project "log"))
  (:log-debug (:project :log "debug.log"))
  (:log-error (:project :log "error.log"))
  ;; texture-test
  (:texture-test-textures (:project :texture "texture-test"))
  (:1da-test (:project :texture-test-textures "1d-array/test"))
  (:2da-test (:project :texture-test-textures "2d-array/test"))
  (:3d-test (:project :texture-test-textures "3d/test"))
  (:cubemap-test (:project :texture-test-textures "cube-map/test"))
  ;; sprite-test
  (:spritesheet (:project :texture "sprite-test/sprites.tga"))
  (:spritesheet-data (:project "sprites.sexp"))
  ;; damaged helmet
  (:damaged-helmet-textures (:project :texture "damaged-helmet")))
