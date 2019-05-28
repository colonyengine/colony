# first-light

A component based game engine written in Common Lisp for modern OpenGL (4.3+).

NOTE: This engine is still in construction, however, it is almost
ready for game making. The developers (psilord and mfiano) hang
out in #lispgames on freenode, and specifically ##hacker-theory
on freenode. Stop by if you want to help or see how to run what
we currently have, or just to say hello.

## Overview

Writing a game is a difficult thing. So we've created a system and
work flow that helps you describe the elements needed to write 2d or
3d games. We designed several domain specific languages that make it
easier to describe, manipulate, and use assets commonly found in game
making. Such assets are, but not limited to, textures, materials,
shader programs, and scene trees of actors that are available for
instantiation. first-light also knows how to accept input from keyboards
and most joysticks and gamepads.

The component system is a hybrid model between an ECS and an object
model.  The components are defined similar to CLOS defclass, and
regular generic methods can be used with them. Components are added to
Actors which represent game concepts like players, scenery, effects,
etc. We define a component protocol invoked by the first-light engine
to move your components to the next state and render them each frame.

## Install

This system is not yet available to be installed automatically with
Quicklisp. To manually install using Quicklisp, clone this repository
into your local-projects directory.

## Usage

To start an example that is already present in first-light,
issue the following in your REPL:

```lisp
(ql:quickload :first-light.example)
(fl:start-engine :scene 'fl.example::damaged-helmet)

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

## 3rd Party Assets and Graphics released with first-light.

### Spaceship Construction Kit
 - License: http://creativecommons.org/licenses/by-sa/3.0/
 - Author: Skorpio
 - Location: https://opengameart.org/content/space-ship-construction-kit
 - Modifications: Color correction

### Space Game Art Pack (Extended)
 - License: http://creativecommons.org/licenses/by-sa/3.0/
 - Author: Tatermand
 - Location: https://opengameart.org/content/space-game-art-pack-extended
 - Modifications: None

### 2D Shooter Effects (Alpha version)
 - License: https://creativecommons.org/publicdomain/zero/1.0/
 - Author: Tatermand
 - Location: https://opengameart.org/content/2d-shooter-effects-alpha-version

### Lasers and beams
 - License: http://creativecommons.org/publicdomain/zero/1.0/
 - Author: Rawdanitsu
 - Location: https://opengameart.org/content/lasers-and-beams

### 20 planet sprites
 - License: http://creativecommons.org/licenses/by/3.0/
 - Author: Justin Nichol
 - Location: https://opengameart.org/content/20-planet-sprites

### Space Backgrounds
 - License: http://creativecommons.org/publicdomain/zero/1.0/
 - Author: Rawdanitsu
 - Location: https://opengameart.org/content/space-backgrounds-3

### 2D Planets
 - License: http://creativecommons.org/publicdomain/zero/1.0/
 - Author: Rawdanitsu
 - Location: https://opengameart.org/content/2d-planets-0

### Sci-fi effects
 - License: http://creativecommons.org/licenses/by/3.0/
 - Author: Skorpio
 - Location: https://opengameart.org/content/sci-fi-effects
 - Modifications: Resizing

### Space Ship & Mech Construction Kit 2
 - License: http://creativecommons.org/licenses/by-sa/3.0/
 - Author: Skorpio
 - Location: https://opengameart.org/content/space-ship-mech-construction-kit-2
 - Modifications: Color correction

### Asteroids
 - License: http://creativecommons.org/licenses/by-sa/3.0/
 - Author: phaelax
 - Location: https://opengameart.org/content/asteroids
 - Modifications: None