# Roadmap

This document describes, in general order, the features we're working on
in order to provide a fully functional engine. The tasks being worked on
are grouped into named milestones. The milestone are ordered with the
first one being the one we are working on and onwards.

The milestones only describes feature that we want completed, not how to
do them in parallel or if they can be done parallel.

NOTE: Completing BlueJay and Grackle will allow the engine to generally
be used by everyday people who like adventures while using their tools.
Additional completion of Great Kiskadee adds the appdev UI functionality
and generally presents a plausible "complete" environment for game
making.


- Milestone: Bluejay
  - Finish branch psilord/define-texture-map
    - Complete recification
    - Complete realiztion
    - Complete define-texture change to use new define-teture-map names.
    - Record stuff left over not otherwise fixed.
  - Complete GLTF package
    - Implement read/write to/from a stream/disk/etc
    - Ensure the runtime API makes sense and is usable to construct them.
    - Document API
    - Ensure can load properly into the resource cache (models, textures, etc)
  - Integrate GLTF with resource-cache and define-prefab DSL
    - When we're close, need to do a little empirical research
    - This enables appdevs to have a library of objects and an simple
      means of linking to them in the define-prefab language.

- Milestone: Grackle
  - Implement a basic audio system
    - Design DSL, programmatic API for audio data structures
    - Internal engine systems and code to play/record the audio
    - appdev define-component library for audio concepts

- Milestone: Great Kiskadee
  - Design UI system for appdevs to use
    - Out of the box supports multiple pointers and interaction
      clipping related to view volume, etc

- Milestone: Cardinal
  - Possibly evolve Graph DSL (one use of: specify animation state
    machines)
  - Normalize GLTF data (it can be dirty from 3d prog/procedural generation).
    - Determine what is "normal".
  - Implement GLTF animations
    - animation state machine
    - animation blending

- Milestone: Sparrow
  - Networking support
    - client/server apps.
    - collaborative editing.
