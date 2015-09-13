package com.blevinstein.qt.grow

import com.blevinstein.qt.Material
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}
import com.blevinstein.util.Decider

// TODO: add MaterialPool/MaterialPalette
object ReplacementRule {
  def update(rules: List[ReplacementRule])(root : QuadTree): QuadTree = {
    def update_recur(qt: QuadTree): QuadTree = {
      val relevantRules = rules filter (_.before == qt)
      if (!relevantRules.isEmpty) {
        // make random replacement
        Decider.choose(relevantRules).after
      } else {
        qt match {
          case branch: QuadBranch => branch.map((tree, _) => update_recur(tree))
          case leaf: QuadLeaf => leaf
        }
      }
    }
    update_recur(root)
  }

  def createRandom: ReplacementRule =
    new ReplacementRule(randomNode, randomNode)

  /**
   * Creates a random node for use in a replacement rule. Only generates simple
   * nodes (leaf or single-level branch), more complex rules must be created
   * through mutation.
   */
  val leafProbability = 0.25f
  def randomNode: QuadTree =
    if (Decider.withProb(leafProbability)) {
      new QuadLeaf(randomMaterial)
    } else {
      QuadBranch.create((quadrant) => new QuadLeaf(randomMaterial))
    }

  // TODO: add more materials
  def randomMaterial: Material =
    Decider.choose(List(Material.Empty, Material.Full))
}
class ReplacementRule(val before: QuadTree, val after: QuadTree) {
  override def toString: String = s"ReplacementRule($before => $after)"
}

