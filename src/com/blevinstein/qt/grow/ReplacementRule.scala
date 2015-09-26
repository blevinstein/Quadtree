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
   *
   * TODO: consider refactoring out of ReplacementRule
   */
  def update(rules: List[ReplacementRule])
      (root : QuadTree[Material]): QuadTree[Material] = {
    def update_recur(qt: QuadTree[Material]): QuadTree[Material] = {
      val results = (rules map (_(qt))).flatten
      if (!results.isEmpty) {
        Decider.choose(results)
      } else {
        qt match {
          case branch: QuadBranch[Material] => branch.map((tree, _) => update_recur(tree))
          case leaf: QuadLeaf[Material] => leaf
        }
      }
    }
    update_recur(root)
  }

  /**
   * Creates a random replacement rule. Only generates simple rules, more
   * complex rules must be created through mutation.
   * TODO: tweak weights
   * TODO: refactor random creator functions out of ReplacementRule
   */
  def randomRule: ReplacementRule = new ReplacementRule(randomTransformTree)
  val leafWeight = 3
  val branchWeight=  1
  val wildcardWeight = 3
  def randomTransformTree: QuadTree[Material => Option[Material]] =
      Decider.chooseWithWeight(
        List('leaf, 'branch, 'wildcard),
        List(leafWeight, branchWeight, wildcardWeight)) match {
    case 'leaf => new QuadLeaf(randomTransform)
    case 'branch => new QuadBranch(randomTransformTree, randomTransformTree,
      randomTransformTree, randomTransformTree)
  }
  def randomTransform: Material => Option[Material] = Decider.choose(List(
      ChangeMaterial(randomMaterial, randomMaterial),
      MatchMaterial(randomMaterial),
      AnyMaterial()))

  // TODO: add more materials
  def randomMaterial: Material =
    Decider.choose(List(Material.Empty, Material.Full))
}
class ReplacementRule(rule: QuadTree[Material => Option[Material]]) {
  class NoMatchException extends RuntimeException
  def apply(tree: QuadTree[Material]): Option[QuadTree[Material]] = {
    val tryFunc = QuadTree.merge(
      (m: Material, f: Material => Option[Material]) => f(m) match {
        case Some(m) => m
        case None => throw new NoMatchException()
      }) _
    try {
      Some(tryFunc(tree, rule))
    } catch {
      case e: NoMatchException => None
    }
  }
}

// Simple transform functions
// TODO: add conditional behavior (e.g. fill space iff empty)
object ChangeMaterial {
  def apply(before: Material, after: Material): Material => Option[Material] =
      (m: Material) =>
        if (m == before) Some(after) else None
}
object MatchMaterial {
  def apply(material: Material): Material => Option[Material] =
      (m: Material) =>
        if (m == material) Some(m) else None
}
object AnyMaterial {
  def apply(): Material => Option[Material] = (m: Material) => Some(m)
}
