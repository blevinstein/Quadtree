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

C *source* *dest* - CopyTool
X *click* - DeleteTool
G *source* *newRect* - GrowTool

TODO
====

Features
--------

- Use Akka to improve behavior of Driver?

- Implement persistence, save/load world

- Add configurable settings

Cleanup
-------

- Add message to every require() call

