Quadtree
========

An implementation of Quadtrees (2D analog of 3D Octrees).

To run the "grow" app:
> sbt grow

To run the "sim" app:
> sbt sim

Controls in the sim:

WASD - Move the figure

Tools:

X [click] - DeleteTool

TODO
====

Features
--------

- Use Akka to improve behavior of Driver?

- GrowTool, can click on an object and copy a block into an adjacent open square

- Implement persistence, save/load world

- Add configurable settings

Cleanup
-------

- Add message to every require() call

