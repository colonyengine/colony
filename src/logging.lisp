(in-package :fl.core)

(simple-logger:define-message :info :engine.start
  "Started ~a.")

(simple-logger:define-message :info :engine.quit
  "Stopped ~a.")

(simple-logger:define-message :trace :extension.load
  "Loaded extension (~(~a~)): ~a.")

(simple-logger:define-message :info :display.init
  "Display ~dx~d @ ~dHz created.")

(simple-logger:define-message :info :display.stop
  "Display ~dx~d @ ~dHz destroyed.")

(simple-logger:define-message :trace :input.key.down
  "Key pressed: ~a.")

(simple-logger:define-message :trace :input.key.up
  "Key released: ~a.")

(simple-logger:define-message :trace :input.scroll.up
  "Mouse wheel scrolled up.")

(simple-logger:define-message :trace :input.scroll.down
  "Mouse wheel scrolled down.")

(simple-logger:define-message :trace :flow.enter
  "Entering flow: (~a ~a ~a).")

(simple-logger:define-message :trace :flow.state.process
  "Processing flow-state: ~a, exiting: ~a.")

(simple-logger:define-message :trace :flow.call.selector
  "Calling selector function.")

(simple-logger:define-message :trace :flow.call.action.hash
  "Calling action function (hash).")

(simple-logger:define-message :trace :flow.call.action.instance
  "Calling action function (instance).")

(simple-logger:define-message :trace :flow.call.transition
  "Calling transition function.")

(simple-logger:define-message :trace :flow.exit
  "Exiting flow: (~a ~a ~a).")

(simple-logger:define-message :debug :shader.programs.updated
  "Shader programs updated: ~{~s~^, ~}")

(simple-logger:define-message :debug :component.mesh.cache.used
  "Used a cached copy of mesh: ~a.")

(simple-logger:define-message :debug :component.mesh.cache.created
  "Creating a new cached mesh for: ~a.")

(simple-logger:define-message :trace :material.check-uniform
  "Checking material: ~a, using uniform ~s.")

(simple-logger:define-message :trace :material.annotate
  "Annotate material: ~a, uniform: ~a, sampler: ~a, texture ID: ~a.")

(simple-logger:define-message :trace :material.resolve
  "Resolving material: ~a.")

(simple-logger:define-message :trace :material.extension.process
  "Processing material extension: ~a.")
