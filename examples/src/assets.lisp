(in-package #:colony-examples)

(c:define-asset-pool meshes ()
  :base "data/meshes"
  :type glb)

(c:define-asset-pool textures ()
  :base "data/textures"
  :type png)

(c:define-asset-pool mesh-textures ()
  :base "data/textures/mesh"
  :type png)

(c:define-asset-pool ptp-textures ()
  :base "data/textures/protect-the-planets"
  :type png)

(c:define-asset-pool metadata ()
  :base "data/metadata")
