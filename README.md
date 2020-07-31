# Virality Engine

A component based game engine written in Common Lisp for modern OpenGL (4.3+).

NOTE: This engine is still in construction, however, it is almost ready for
game making. The developers (psilord and mfiano) hang out in #bufferswap on
freenode, and sometimes #lispgames on freenode. Stop by if you want to help
or see how to run what we currently have, or just to say hello.

## Overview

Writing a game is a difficult thing. So we've created a system and work flow
that helps you describe the elements needed to write 2d or 3d games. We
designed several domain specific languages that make it easier to describe,
manipulate, and use assets commonly found in game making. Such assets are, but
not limited to, textures, materials, shader programs, and scene trees of actors
that are available for instantiation. Virality Engine also knows how to accept
input from keyboards and most joysticks and gamepads.

The component system is a hybrid model between an ECS and an object model. The
components are defined similar to CLOS defclass, and regular generic methods
can be used with them. Components are added to Actors which represent game
concepts like players, scenery, effects, etc. We define a component protocol
invoked by Virality Engine to move your components to the next state and render
them each frame.

## Install

NOTE: Virality will only work on sbcl 2.0.5 or before. There was some change
in 2.0.6 that causes Virality to not load and we are in the process of
figuring out why.

This system is not yet available to be installed automatically with Quicklisp.

To manually install such that Quicklisp will be able to find ViralityEngine,
clone this repository into your local-projects directory.

For now, being on the `develop` branch will provide a better experience
and newer features.

If you've run ViralityEngine before, and haven't in a while, then

cd ~quicklisp/local-projects

and as long as you aren't using these specifically for yourself:

rm -rf origin doubly-linked-list golden-utils umbra shadow

This next shell script is recommended to help with the depdenencies of
ViralityEngine.

Put this bash script into local-projects, you might call it
'update-virality-depdendencies.sh' or something similar.

```
#! /bin/bash

echo "Updating shadow..."
if [ ! -d ./shadow ]; then
	git clone https://github.com/mfiano/shadow.git
fi
(cd shadow && git pull)

echo "Updating umbra..."
if [ ! -d ./umbra ]; then
	git clone https://github.com/mfiano/umbra.git
fi
(cd umbra && git pull)

echo "Updating golden-utils..."
if [ ! -d ./golden-utils ]; then
	git clone https://github.com/mfiano/golden-utils.git
fi
(cd golden-utils && git pull)

echo "Updating algae..."
if [ ! -d ./algae ]; then
	git clone https://github.com/mfiano/algae.git
fi
(cd algae && git pull)
```

Then chmod 700 the script and run it while in the ~quicklisp/local-projects
directory.

Every now and then, when pulling ViralityEngine, ensure to re-run that
shell script to get any matching code changes. It is expected that those
dependencies will work generally only with ViralityEngine on the 'develop'
branch.

## Usage

To start an example that is already present in Virality Engine, issue the
following in your REPL:

```lisp
(ql:quickload :virality-examples)

(in-package :virality-examples)

;; To show the GLTF damaged helmet (mouse will drag and move it around.)
(virality:start
       :project :virality-examples
       :scene '(("damaged-helmet-interactive" examples)))

;; To run the Protect the Planets game (requires a gamepad to play)
;; Directions:
;; Press Start to play.
;; Use d-pad to move around 8-way.
;; Hold right shoulder to pivot while moving.
;; Hold left shoulder for half-speed.
;; Hold A (or whatever mapped to A) button to fire.
;; Esc on keyboard quits.
(virality:start
       :project :virality-examples
       :scene '(("protect-the-planets" ptp)))

;; To run some interesting art that mfiano
;; ported from shader-toy and hand modified.
;;
;; NOTE: Use mouse (drag LMB) to look around!
(virality:start
       :project :virality-examples
       :scene '(("art6" examples)))

;; ESC exits
```

There are many more examples.

## License

Copyright © 2017-2020

* Michael Fiano <mail@mfiano.net>

* Peter Keller <psilord@cs.wisc.edu>

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
