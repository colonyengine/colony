# Virality Engine

A component based game engine written in Common Lisp for modern OpenGL (4.3+).

NOTE: This engine is still in construction--but is far enough that certain
simple classes of games can be made with it.  The developers (`psilord`,
`mfiano`, `|3b|`, and `jgl`) hang out in #bufferswap on irc.libera.chat, and
sometimes #lispgames on irc.libera.chat. Stop by if you want to help or see how
to run what we currently have, or just to say hello.

## Overview

Writing a game is a difficult thing. So we've created a system and workflow
that helps you describe the elements needed to write 2D or 3D games. We
designed several domain specific languages that make it easier to describe,
manipulate, and use assets commonly found in game making. Such assets include
(but are not limited to) textures, materials, shader programs, and scene trees
of actors that are available for instantiation. Virality Engine also knows how
to accept input from keyboards and most joysticks and gamepads.

The component system is a hybrid model between an ECS and an object model. The
components are defined similar to CLOS `defclass`, and regular generic methods
can be used with them. Components are added to Actors which represent game
concepts like players, scenery, effects, etc. We define a component protocol
invoked by Virality Engine to move your components to the next state and render
them each frame.

## Install

Virality Engine is not yet available in Quicklisp, but you can use Quicklisp to
load it once you clone it.

NOTE: `cl-opengl` has a feature in it to work around an Intel GPU bug that
causes a severe performance problem in most cases where people aren't using the
buggy Intel driver. While you aren't required to perform the actions in this
note, if you do, Virality Engine's performance will be much better. Virality
Engine disables this feature in Virality Engine's ASD file.  However, if there
are `cl-opengl` FASLs which have been pre-cached, they need to be
recompiled. So, first recursively remove `~/.cache/common-lisp/*` or wherever
you store your FASLs.  Then, ensure that Virality Engine is _FIRST_ in any
`:depends-on` line for your Virality Engine projects. Then, load a Virality Engine project as
the first thing you do with in a REPL with the removed FASLs, so that
`cl-opengl` is required by Virality Engine's ASD, which will turn off the feature. If
you try and load something other than Virality Engine that requires `cl-opengl`, then
the feature won't be turned off and the performance problem will still
happen. `cl-opengl` will probably be fixed to change this behavior in a future
commit, but until then this is a fix for a performance problem.

To manually install such that Quicklisp will be able to find
Virality Engine, clone this repository into your local-projects directory.

```
cd ~/quicklisp/local-projects
git clone https://github.com/bufferswap/ViralityEngine.git
cd ViralityEngine
git checkout develop
```

**NOTE: You may have better luck with the `develop` branch for the time being!
The `develop` branch does periodically synch with master, but master is often
crusty.**

If you've cloned and run Virality Engine before Oct 29th, 2022 then as long as
you aren't using these specifically for yourself, remove these directories:

```
cd ~/quicklisp/local-projects
rm -rf origin doubly-linked-list golden-utils mfiano-utils umbra shadow origin algae
```

If you have one of those directories and want to keep it, Virality Engine will
not conflict with it--but it is good to remove detritus from previous versions.

After Oct 29th, 2022, Virality Engine subsumed a collection of what used to be
seperate (but highly related) libraries into itself to decrease maintenance
costs and make it easier to evolve the game engine. This makes Virality Engine
a single checkout codebase that just uses common (or slowly changing) libraries
in Quicklisp. It also makes it much easier for other users of the engine to
just use it for a project.

## Usage

**NOTE: SBCL's default memory heap size is insufficient to run all the
examples. Please ensure that your SBCL (either on the console, or in your SLIME
or SLY configuration) is started with the command line argument
`--dynamic-space-size 24000` (24GB) or a memory amount that is the size of your
RAM on your machine. Leaving it at the default value will cause Virality Engine
to eventually fail with a heap exhaustion error (often while loading
textures).**

To start an example that is already present in Virality Engine, issue the
following in your REPL:

```lisp
;; Done once after cloning to ensure Quicklisp finds all Virality Engine
;; ASD files.
(ql:register-local-projects)

(ql:quickload :virality-examples)

(in-package :virality-examples)


;; ESC will exit the engine when running these examples....


;; To show the GLTF damaged helmet (mouse will drag and move it around.)
(virality:start
       :config 'virality-examples
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
       :config 'virality-examples
       :scene '(("protect-the-planets" ptp)))

;; To run some interesting art that mfiano
;; ported from shader-toy and hand modified.
;;
;; NOTE: Use mouse (drag LMB) to look around!
(virality:start
       :config 'virality-examples
       :scene '(("art6" examples)))

;; To run a menu selector for all examples including those above:
;; And ensure to pay attention to the keyboard interface to move in and
;; out of the examples.
;;
;; NOTE: Due to an unfinished feature the sprite example and the
;; protect-the-planets example cannot be run in the same session. You'll have
;; to exit Virality Engine and restart it if you ran either the
;; protect-the-planets example, or the sprite example, and would like to run
;; the other one.
(virality:start
       :config 'virality-examples
       :scene '(("example-selector" examples)))

;; ESC exits

;; A project may have multiple configurations when it executes, such as
;; different resolutions, physics update speeds, or different prefabs in the
;; initial scene, etc, etc.  To find and start the default configuration for a
;; project, simply:
(virality:start)

```

The example selector shows many examples.

# Test Suite

The test suite will eventually test all layers and components of the internal
engine codes and the support libraries. We currently use `parachute` for our
testing methodology.

To run the Virality Engine test suite, run this at the repl:
```
(asdf:test-system :virality)
```


## License

Copyright © 2017-2022

* Bart Botta <00003b at gmail.com>

* Michael Fiano <mail@mfiano.net>

* Peter Keller <psilord@cs.wisc.edu>

* Jack Ladwig <ladwi035@gmail.com>

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
