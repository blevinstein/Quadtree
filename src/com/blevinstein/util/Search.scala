package com.blevinstein.util

import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet

/**
 * This class contains implementations of generic search algorithms, such as
 * BFS/DFS, floodfill, or A*.
 */
object Search {
  // DFS floodfill
  def floodfill[Node](start: Node, neighbors: Node => Iterable[Node]):
      List[Node] = {
    val visited = new HashSet[Node]()
    val stack = new Stack[Node]()
    var result = List[Node]()

    stack.push(start)
    visited.add(start)

    while (!stack.isEmpty) {
      val current = stack.pop()
      for (neighbor <- neighbors(current)) {
        if (!visited.contains(neighbor)) {
          stack.push(neighbor)
          visited.add(neighbor)
        }
      }
      result = current :: result
    }

    result
  }
}
