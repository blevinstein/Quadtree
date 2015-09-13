package com.blevinstein.qt.grow

import com.blevinstein.qt.Material
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}
import com.blevinstein.util.Decider

// TODO: add MaterialPool/MaterialPalette
object ReplacementRule {
  /**
   * Given a list of ReplacementRules, upate the given QuadTree. Tries to apply
   * rules at the highest depth possible. If multiple rules are applicable at
   * the same depth, randomly chooses one to apply.
   */
  def update(rules: List[ReplacementRule])(root : QuadTree): QuadTree = {
    def update_recur(qt: QuadTree): QuadTree = {
      val relevantRules = rules filter (_.before == qt)
      if (!relevantRules.isEmpty) {
        // choose between applicable rules randomly
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

  /**
   * Creates a random replacement rule. Only generates simple rules, more
   * complex rules must be created through mutation.
   */
  def createRandom: ReplacementRule =
    new ReplacementRule(randomNode, randomNode)

  /**
   * Creates a random node for use in a replacement rule. Only generates simple
   * nodes (leaf or single-level branch).
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

