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
  def randomRule: ReplacementRule =
    Decider.chooseWithWeight(List('leaf, 'branch, 'wildcard),
        List(3, 1, 3)) match {
      case 'leaf => randomLeafRule
      case 'branch => randomBranchRule
      case 'wildcard => WildcardRule
    }

  def randomLeafRule: LeafRule = new LeafRule(Decider.choose(List(
      ChangeMaterial(randomMaterial, randomMaterial),
      MatchMaterial(randomMaterial),
      AnyMaterial())))

  def randomBranchRule: BranchRule = new BranchRule(
      randomRule, randomRule, randomRule, randomRule)

  // TODO: add more materials
  def randomMaterial: Material =
    Decider.choose(List(Material.Empty, Material.Full))
}
trait ReplacementRule {
  def apply(tree: QuadTree[Material]): Option[QuadTree[Material]]
}

class BranchRule(a: ReplacementRule, b: ReplacementRule, c: ReplacementRule,
    d: ReplacementRule) extends ReplacementRule {
  def apply(tree: QuadTree[Material]): Option[QuadTree[Material]] = tree match {
    case branch: QuadBranch[Material] => {
      // Apply all sub-rules
      val maybeA = a.apply(branch.a)
      val maybeB = b.apply(branch.b)
      val maybeC = c.apply(branch.c)
      val maybeD = d.apply(branch.d)
      // All sub-rules must match
      (maybeA, maybeB, maybeC, maybeD) match {
        case (Some(newA), Some(newB), Some(newC), Some(newD)) =>
            Some(new QuadBranch(newA, newB, newC, newD).tryMerge)
        case _ => None
      }
    }
    case _ => None
  }

  override def toString: String = s"BranchRule($a, $b, $c, $d)"
}

class LeafRule(f: Material => Option[Material]) extends ReplacementRule {
  def apply(tree: QuadTree[Material]): Option[QuadTree[Material]] = tree match {
    case leaf: QuadLeaf[Material] => f(leaf.data) match {
      case Some(newMaterial) => Some(new QuadLeaf(newMaterial))
      case _ => None
    }
    case _ => None
  }

  override def toString: String = s"LeafRule($f)"
}

object WildcardRule extends ReplacementRule {
  def apply(tree: QuadTree[Material]): Option[QuadTree[Material]] = Some(tree)

  override def toString: String = s"WildcardRule"
}


// Simple LeafRule functions
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
