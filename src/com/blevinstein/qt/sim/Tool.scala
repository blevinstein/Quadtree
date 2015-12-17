package com.blevinstein.qt.sim

object Tool {
  val Noop: ToolOutput = (List(), List())
}
trait Tool {
  // This method returns [Drawable]s to draw on the screen, e.g. a cursor
  // or text messages to the user, as well as [Event]s that change the state of
  // the world.
  def activate(world: World, input: List[Input]): ToolOutput

  // When [clear] returns true, [inputStack] is cleared.
  // TODO: refactor into activate()? It's annoying that the return type of
  //   [activate] is getting so long. If it gets any bigger than 3 parts, I
  //   should create a class ToolOutput.
  def clear(world: World, input: List[Input]): Boolean
}

