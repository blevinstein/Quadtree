package com.blevinstein.qt.grow

import com.blevinstein.qt.QuadAddr
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}
import com.blevinstein.qt.Quadrant.{TopLeft,TopRight,BottomLeft,BottomRight}
import com.blevinstein.util.RateLimiter

// Defines a simple method for growing a QuadTree, given a QuadGenome, which is
// a List of ReplacementRules, which can be applied to transform a QuadTree.
//
// The GrowthSim will start with a single [true] square in the midst of a field
// of [false] squares. Each update step chooses a random applicable
// ReplacementRule to apply in each leaf node of the tree.
//
// Growth is limited by number of steps and by execution time.
object GrowthSim {
  val environmentSize = 5 // environment side length = (2^environmentSize)
  val startAddress =
      new QuadAddr(BottomLeft ::
        List.tabulate(environmentSize - 1)((_) => TopRight))

  /*
   * Given a genome, grows a QuadTree
   * Returns (resulting QuadTree, number of steps to grow)
   */
  def apply(genome: QuadGenome): (QuadTree[Boolean], Int) = {
    var currentTree = new QuadTree.Builder[Boolean](false)
        .add(startAddress, true).build
    var steps = 0
    var done = false
    val maxSteps = 100
    val maxMillis = 100
    val maxGrowth = new RateLimiter(maxMillis)
    while (!done && steps < maxSteps && !maxGrowth.check) {
      val newTree = ReplacementRule.update(genome)(currentTree)
      if (newTree == currentTree) {
        done = true
      } else {
        steps += 1
        currentTree = newTree
      }
    }
    (currentTree, steps)
  }
}
