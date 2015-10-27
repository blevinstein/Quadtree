package com.blevinstein.qt.grow

import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}
import com.blevinstein.util.Decider

// The main method of a ReplacmentRule is apply. When an ReplacementRule is
// applied to a QuadTree[Boolean], it either returns None (if this rule cannot
// be applied), or Some[QuadTree[Boolean]], the result of applying this
// ReplacementRule to the given QuadTree.
//
// The structure of the ReplacementRule is, itself, a QuadTree, with its leaf
// nodes containing QuadTransform = Boolean => Option[Boolean]. For a
// ReplacementRule to apply, every leaf node must evaluate to Some[Boolean].
object ReplacementRule {
  /**
   * Given a list of ReplacementRules, upate the given QuadTree. Tries to apply
   * rules at the highest depth possible. If multiple rules are applicable at
   * the same depth, randomly chooses one to apply.
   *
   * TODO: consider refactoring out of ReplacementRule
   */
  def update(rules: List[ReplacementRule])
      (root : QuadTree[Boolean]): QuadTree[Boolean] = {
    def update_recur(qt: QuadTree[Boolean]): QuadTree[Boolean] = {
      val results = (rules map (_(qt))).flatten
      if (!results.isEmpty) {
        Decider.choose(results)
      } else {
        qt match {
          case branch: QuadBranch[Boolean] => branch.map((tree, _) => update_recur(tree))
          case leaf: QuadLeaf[Boolean] => leaf
        }
      }
    }
    update_recur(root)
  }

  /**
   * Creates a random replacement rule. Only generates simple rules, more
   * complex rules must be created through mutation.
   */
  def randomRule: ReplacementRule = new ReplacementRule(randomTransformTree)
  def randomTransformTree: QuadTree[QuadTransform] =
      Decider.chooseWithWeight(
        List('leaf, 'branch),
        List(6, 1)) match { // scalastyle:off magic.number
    case 'leaf => new QuadLeaf(randomTransform)
    case 'branch => new QuadBranch(randomTransformTree, randomTransformTree,
        randomTransformTree, randomTransformTree)
  }
  def randomTransform: QuadTransform = Decider.choose(List(
      ChangeMaterial(randomMaterial, randomMaterial),
      MatchMaterial(randomMaterial),
      AnyMaterial()))

  // TODO: add more materials
  def randomMaterial: Boolean =
    Decider.choose(List(false, true))
}
class ReplacementRule(rule: QuadTree[QuadTransform]) {
  class NoMatchException extends RuntimeException
  def apply(tree: QuadTree[Boolean]): Option[QuadTree[Boolean]] = {
    val tryFunc = QuadTree.merge(
      (m: Boolean, f: QuadTransform) => f(m) match {
        case Some(m) => m
        case None => throw new NoMatchException()
      }) _
    try {
      Some(tryFunc(tree, rule))
    } catch {
      case e: NoMatchException => None
    }
  }

  def mutate: ReplacementRule = {
    def mutate_recur(tree: QuadTree[QuadTransform]):
        QuadTree[QuadTransform] = {
      tree match {
        case branch: QuadBranch[QuadTransform] =>
            Decider.chooseWithWeight(
              List('recur, 'replace),
              List(1, 1)) match {
          case 'replace => ReplacementRule.randomTransformTree
          case 'recur => Decider.choose(List('a, 'b, 'c, 'd)) match {
            case 'a => new QuadBranch(mutate_recur(branch.a), branch.b,
                branch.c, branch.d)
            case 'b => new QuadBranch(branch.a, mutate_recur(branch.b),
                branch.c, branch.d)
            case 'c => new QuadBranch(branch.a, branch.b,
                mutate_recur(branch.c), branch.d)
            case 'd => new QuadBranch(branch.a, branch.b,
                branch.c, mutate_recur(branch.d))
          }
        }
        case leaf: QuadLeaf[QuadTransform] =>
            new QuadLeaf(ReplacementRule.randomTransform)
      }
    }
    new ReplacementRule(mutate_recur(rule))
  }
}

// Simple transform functions
// TODO: add conditional behavior (e.g. fill space iff empty)
// TODO: to make tryMerge work, these function factories should be memoized
object ChangeMaterial {
  def apply(before: Boolean, after: Boolean): QuadTransform =
      (m: Boolean) =>
        if (m == before) Some(after) else None
}
object MatchMaterial {
  def apply(material: Boolean): QuadTransform =
      (m: Boolean) =>
        if (m == material) Some(m) else None
}
object AnyMaterial {
  def apply(): QuadTransform = (m: Boolean) => Some(m)
}

