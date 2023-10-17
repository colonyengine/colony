Game Engines have a lot of complex moving parts. This reference section will
help you find knowledge about what algorithms we use, the methodologies of
thought behind some of our designs, and general knowledge about engine design,
mathematics, algorithms, etc.

Every book, paper, or other that we find that helps us in our quest shall be
recorded here. We try to grade the references from easiest to hardest in each
section for easier learning of the material. These set of references is an ever
growing work.

# Common Lisp

 - Seibel, Peter (2005). [Practical Common Lisp][PCL-Seibel]. Springer Nature: Apress. ISBN 978-1-59059-239-7.
 - Keene, Sonya (1989). [Object-Oriented Programming in COMMON LISP][OOPCL-Keene]. Addison-Wesley Professional. ISBN 978-0201175899.
 - Hoyte, Doug (2008). [Let Over Lambda][LOL-Hoyte]. Lulu.com. ISBN 1435712757. [Errata][LOL-Err-Hoyte]
 - Weitz, Edmund (2015). [Common Lisp Recipes: A Problem-Solution Approach][CLR-Weitz]. Apress. ISBN 1484211774. [Errata][CLR-Err-Weitz]
 - Norvig, Peter (1992). [Paradigms of Artificial Intelligence Programming][PAIP-Norvig]. Morgan Kaufmen Publishers. ISBN 1-55860-191-0. [Errata][PAIP-Err-Norvig]
 - [Common Lisp Hyperspec][CLHS].

# Game Engine Design and Methodology

 - Gregory, Jason (2018). [Game Engine Architecture, Third Edition][GEA3-Gregory]. CRC Press. ISBN 978-1-13803-545-4

# Mathematics

There is a lot of different kind of mathematics in a game engine. A small
survey of the interesting areas are: linear algebra (matrix math, projective
geometry, rigid body transformations, etc), computational geometry (mesh
generation, signed distance functions, voronoi maps, etc), noise (turbulence,
procedural generation, Perlin noise, etc), object intersections (bounding box
against bounding box, sphere against bounding box, etc), physics simulation
(kinematics, inverse kinematics, fluid, car steering, etc), etc. As this
section grows, we will categorize a reference by the main topic it talks about
(or for what we used it), even though it likely could go into multiple
different sections.

 - Ericson, Christer (2004). [Real-Time Collision Detection][RTCD-Ericson]. CRC Press. ISBN 1558607323. [Errata][RTCD-Err-Ericson]

# Algorithms

 - Cormen, Thomas H. and Leiserson, Charles E. and Rivest, Ronald L. and Stein, Clifford (2009). [Introduction to Algorithms, Third Edition][IA-Cormen]. The MIT Press. ISBN 0262033844 [Errata][IA-Err-Cormen]















[PCL-Seibel]: https://gigamonkeys.com/book/
[LOL-Hoyte]: https://letoverlambda.com/
[LOL-Err-Hoyte]: https://letoverlambda.com/index.cl/errata
[CLR-Weitz]: http://weitz.de/cl-recipes/
[CLR-Err-Weitz]: http://weitz.de/cl-recipes/errata.pdf
[GEA3-Gregory]: https://www.gameenginebook.com/
[RTCD-Ericson]: https://realtimecollisiondetection.net/
[RTCD-Err-Ericson]: https://realtimecollisiondetection.net/books/rtcd/errata/
[CLHS]: http://www.lispworks.com/documentation/HyperSpec/Front/index.htm
[IA-Cormen]: https://mitpress.mit.edu/9780262533058/introduction-to-algorithms/
[IA-Err-Cormen]: https://www.cs.dartmouth.edu/~thc/clrs-bugs/bugs-3e.php
[PAIP-Norvig]: https://github.com/norvig/paip-lisp
[PAIP-Err-Norvig]: https://norvig.com/paip-errata.html
[OOPCL-Keene]: [https://www.amazon.com/Object-Oriented-Programming-COMMON-LISP-Programmers/dp/0201175894]
