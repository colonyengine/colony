# Software Layers in the Engine

This is a work in progress.

The purpose of this document is to give a high level overview of the software
layers of Colony (usually represented by packages and systems) and how they
stand in relation to each other. The layers in this document usually contain
packages (with an assumed `colony.` infront of them). which are built on top of
other packages. If there is a (s) after the name, it denotes a system and all
packages in it. Partitions inside of a layer are themselves smaller layers
built on top of those below them. The bottom of the layer diagram is the lowest
layer closest to the hardware and the top layer is the most abstract. The
description is coarse in the sense that the exact dependencies are not
specified. Packages in the same layer must never depend (read: call into or be
called from) on any other package in that layer.

We expect to reorganize and continue to break stuff into packages.

- Layer: Colony
    - This layer is a mudball of everything else at all abstraction layers. It
      is continuously being broken apart and refactored into additional layers.

- Layer: Asset Kinds
    - texture
    - texture-map

- Layer: Core Early
    - attribute-bag
    - uuid thread-pool resource-cache image
    - clone

- Layer: Support Libraries
    - vorigin(s) vshadow(s)
    - vumbra(s)
    - vutils(s)

- Layer: System Dependencies
    - See the :depends-on for in colony.asd and colony.test.asd.
