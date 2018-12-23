(in-package :first-light.example)

(fl:define-options ()
  :title "First-Light Examples"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug
  :log-repl-categories '(:fl))

(fl:define-resources (:project :first-light.example)
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
  (:cubemaparray-test (:project :texture-test-textures "cube-map-array"))
  ;; sprite-test
  (:spritesheet (:project :texture "sprite-test/sprites.tiff"))
  (:spritesheet-data (:project "sprites.sexp"))
  ;; damaged helmet
  (:damaged-helmet-textures (:project :texture "damaged-helmet")))

;;; TODO: Possibly make prologue and epilogue DSLs.

(defun prologue (context)
  (declare (ignore context))
  (v:trace :fl.core.engine "Running prologue method."))

(defun epilogue (context)
  (declare (ignore context))
  (v:trace :fl.core.engine "Running epilogue method."))

;;; TODO: Fix graphs to work in user package

(in-package :%first-light)

(fl:define-graph :fl.example
    (:category component-dependency
     :depends-on ((:core (all-unknown-types core-types)))
     :roots (all-ordered-types))
  (subdag all-ordered-types
          ((splice core-types)
           -> (splice all-unknown-types))))

(fl:define-graph :fl
    (:category component-package-order
     :depends-on ((:core-component-order (core-packages)))
     :roots (start-search))
  (subdag current-project (:fl.example.comp.* -> :fl.example))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
