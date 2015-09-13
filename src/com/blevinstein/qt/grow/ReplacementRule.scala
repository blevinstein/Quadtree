package com.blevinstein.qt.grow

import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}
import com.blevinstein.util.Decider

// TODO: add ReplacementRule.createRandom
// TODO: add ReplacementRule.randomNode
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
}
class ReplacementRule(val before: QuadTree, val after: QuadTree)

