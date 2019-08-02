# Virality Engine

A component based game engine written in Common Lisp for modern OpenGL (4.3+).

NOTE: This engine is still in construction, however, it is almost ready for game
making. The developers (psilord and mfiano) hang out in #lispgames on freenode,
and specifically ##hacker-theory on freenode. Stop by if you want to help or see
how to run what we currently have, or just to say hello.

## Overview

Writing a game is a difficult thing. So we've created a system and work flow
that helps you describe the elements needed to write 2d or 3d games. We designed
several domain specific languages that make it easier to describe, manipulate,
and use assets commonly found in game making. Such assets are, but not limited
to, textures, materials, shader programs, and scene trees of actors that are
available for instantiation. Virality Engine also knows how to accept input from
keyboards and most joysticks and gamepads.

The component system is a hybrid model between an ECS and an object model. The
components are defined similar to CLOS defclass, and regular generic methods can
be used with them. Components are added to Actors which represent game concepts
like players, scenery, effects, etc. We define a component protocol invoked by
Virality Engine to move your components to the next state and render them each
frame.

## Install

This system is not yet available to be installed automatically with Quicklisp.
To manually install using Quicklisp, clone this repository into your
local-projects directory.

## Usage

To start an example that is already present in Virality Engine, issue the
following in your REPL:

```lisp
(ql:quickload :virality.examples)
(virality-engine:start :scene 'virality.examples:damaged-helmet)

;; ESC exits
```

There are many more examples.

## License

Copyright © 2017-2018

* Michael Fiano <mail@michaelfiano.com>

* Peter Keller <psilord@cs.wisc.edu>

* Bart Botta <00003b@gmail.com>

* Elijah Malaby <djeis>

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
