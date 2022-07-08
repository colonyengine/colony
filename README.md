# Virality Engine

A component based game engine written in Common Lisp for modern OpenGL (4.3+).

NOTE: This engine is still in construction, however, it is almost ready for
making games. The developers (psilord and mfiano) hang out in #bufferswap on
irc.libera.chat, and sometimes #lispgames on irc.libera.chat. Stop by if you want to help
or see how to run what we currently have, or just to say hello.

## Overview

Writing a game is a difficult thing. So we've created a system and workflow
that helps you describe the elements needed to write 2D or 3D games. We
designed several domain specific languages that make it easier to describe,
manipulate, and use assets commonly found in game making. Such assets include (but
are not limited to) textures, materials, shader programs, and scene trees of actors
that are available for instantiation. Virality Engine also knows how to accept
input from keyboards and most joysticks and gamepads.

The component system is a hybrid model between an ECS and an object model. The
components are defined similar to CLOS `defclass`, and regular generic methods
can be used with them. Components are added to Actors which represent game
concepts like players, scenery, effects, etc. We define a component protocol
invoked by Virality Engine to move your components to the next state and render
them each frame.

## Install

**NOTE: You may have better luck with the `develop` branch for the time being! The master
branch is a bit crusty.**

This system is not yet available to be installed automatically with Quicklisp.

NOTE: `cl-opengl` has a feature in it to work around an Intel GPU bug that causes
a severe performance problem in most cases where people aren't using the buggy
Intel driver. While you aren't required to perform the actions in this note, if
you do, Virality's performance will be much better. Virality disables this feature in Virality's
ASD file.  However, if there are `cl-opengl` FASLs which have been pre-cached,
they need to be recompiled. So, first recursively remove `~/.cache/common-lisp/*`
or wherever you store your FASLs.  Then, ensure that Virality is _FIRST_ in any
`:depends-on` line for your Virality projects. Then, load a Virality project as the first thing
you do with in a REPL with the removed FASLs, so that `cl-opengl` is required by
Virality's ASD, which will turn off the feature. If you try and load something other
than Virality that requires `cl-opengl`, then the feature won't be turned off and the
performance problem will still happen. `cl-opengl` will probably be fixed to
change this behavior in a future commit, but until then this is a fix for a
performance problem.


To manually install such that Quicklisp will be able to find ViralityEngine,
clone this repository into your local-projects directory.

For now, being on the `develop` branch will provide a better experience
and newer features. However, it often uses changes to certain dependencies
that are often newer than what Quicklisp provides.

If you've run ViralityEngine before, and haven't in a while, then

`cd ~quicklisp/local-projects`

and as long as you aren't using these specifically for yourself:

`rm -rf origin doubly-linked-list golden-utils umbra shadow origin`

This next shell script is recommended to help with the depdenencies of
ViralityEngine.

Put this bash script into `~/quicklisp/local-projects`, (or wherever your
Quicklisp local-projects directory is) you might call it
`update-virality-depdendencies.sh` or something similar. When updating
Virality from github it is recommended to also run this script to get current
changes Virality may need.


```
#! /bin/bash


echo "Updating origin..."
if [ ! -d ./origin ]; then
	git clone https://github.com/mfiano/origin.git
fi
(cd origin && git pull)

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

Then `chmod 700` the script and run it while in the `~quicklisp/local-projects`
directory.

Every now and then, when pulling ViralityEngine, make sure to re-run that
shell script to get any matching code changes. It is expected that those
dependencies will generally work only with ViralityEngine on the `develop`
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
       :scene '(("damaged-helmet-turn-table" examples)))

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

;; To run a menu selector for all examples including those above:
;; And ensure to pay attention to the keyboard interface to move in and
;; out of the examples.
(virality:start
       :project :virality-examples
       :scene '(("example-selector" examples)))

;; ESC exits
```

There are many more examples.

## License

Copyright © 2017-2020

* Michael Fiano <mail@mfiano.net>

* Peter Keller <psilord@cs.wisc.edu>

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
