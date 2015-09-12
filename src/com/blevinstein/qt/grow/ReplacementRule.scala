package com.blevinstein.qt.grow

import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}

import scala.util.Random

object ReplacementRule {
  // Choose random element from a list
  // TODO: refactor to util
  def choose[T](list: List[T]): T = list.apply(Random.nextInt(list.length))

  def update(rules: List[ReplacementRule])(root : QuadTree): QuadTree = {
    def update_recur(qt: QuadTree): QuadTree = {
      val relevantRules = rules filter (_.before == qt)
      if (!relevantRules.isEmpty) {
        // make random replacement
        choose(relevantRules).after
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

