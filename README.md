# Virality Engine

A component based game engine written in Common Lisp for modern OpenGL (4.3+).

NOTE: This engine is still in construction--but is far enough that certain
simple classes of games can be made with it.  The developers (`psilord`,
`mfiano`, `|3b|`, and `jgl`) hang out in #bufferswap on freenode, and sometimes
#lispgames on freenode. Stop by if you want to help or see how to run what we
currently have, or just to say hello.

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

This system is not yet available to be installed automatically with Quicklisp.

NOTE: cl-opengl has a feature in it to work around an INTEL GPU bug that causes
a severe performance problem in most cases where people aren't using the buggy
intel driver. While you aren't required to perform the actions in this note, if
you do the performance of V will be much better. V disables this feature in V's
asd file.  However, if there are cl-opengl fasls which have been pre-cached,
they need to be recompiled. So, first recursively remove ~/.cache/common-lisp/*
or wherever you store your fasls.  Then ensure that V is _FIRST_ in any
:depends-on line for your V projects. Then, load a V project as the first thing
you do with in a REPL with the removed fasls and cl-opengl will be required by
V's asd which will turn off the feature. If you try and load something other
than V that requires cl-opengl, then the feature won't be turned off and the
performance problem will still happen. cl-opengl will probably be fixed to
change this behavior in a future commit, but until then this is a fix for a
performance problem.

To manually install such that Quicklisp will be able to find ViralityEngine,
clone this repository into your local-projects directory.

``` cd ~/quicklisp/local-projects git clone
https://github.com/bufferswap/ViralityEngine cd ViralityEngine git checkout
develop ```

For now, being on the `develop` branch will provide a better experience and
newer features.

If you've cloned and run ViralityEngine before Oct 29th, 2022 then as long as
you aren't using these specifically for yourself, remove these directories:

```
cd ~/quicklisp/local-projects
rm -rf origin doubly-linked-list golden-utils mfiano-utils umbra shadow origin algae
```

After Oct 29th, 2022, Virality subsumed a collection of what used to be
seperate (but highly related) libraries into itself to decrease maintenance
costs and make it easier to evolve the game engine. This makes Virality a
single checkout codebase that just uses common (or slowly changing) libraries
in Quicklisp. It also makes it much easier for other users of the engine to
just use it for a project.

## Usage

To start an example that is already present in Virality Engine, issue the
following in your REPL:

```lisp
;; Done once to ensure Quicklisp finds all Virality asd files.
(ql:register-local-projects)

(ql:quickload :virality-examples)

(in-package :virality-examples)


;; ESC will exit the engine when running these examples....


;; To show the GLTF damaged helmet (mouse will drag and move it around.)
(virality:start
       :project :virality-examples
       :scene '(("damaged-helmet-turn-table" examples)))

;; To run the Protect the Planets game (requires an xbox-like gamepad to play)
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
;;
;; NOTE: Due to an unfinished feature the sprite example and the
;; protect-the-planets example cannot be run in the same session. You'll have
;; to exit Virality and restart it if you ran either the protect-the-planets
;; example, or the sprite example, and would like to run the other one.
(virality:start
       :project :virality-examples
       :scene '(("example-selector" examples)))

;; ESC exits
```

The example selector shows many examples.

## License

Copyright © 2017-2022

* Bart Botta <00003b at gmail.com>

* Michael Fiano <mail@mfiano.net>

* Peter Keller <psilord@cs.wisc.edu>

* Jack Ladwig <ladwi035@gmail.com>

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
