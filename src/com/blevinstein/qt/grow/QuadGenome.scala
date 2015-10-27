package com.blevinstein.qt.grow

import com.blevinstein.ga.Genome
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}
import com.blevinstein.util.Decider

import scala.language.implicitConversions
import scala.util.Random

// Provides a Genome for running a genetic algorithm.
//
// Each gene is a ReplacementRule.
object QuadGenome {
  // delegate to rules
  implicit def toRuleList(genome: QuadGenome): List[ReplacementRule] =
      genome.rules

  def create(size: Int): QuadGenome = new QuadGenome(
      List.tabulate(size)((_) => ReplacementRule.randomRule))
}
class QuadGenome(val rules: List[ReplacementRule]) extends Genome[QuadGenome] {
  val growProbability = 0.05f
  def mutate: QuadGenome = {
    if (Decider.withProb(growProbability)) {
      new QuadGenome(rules :+ ReplacementRule.randomRule)
    } else {
      val index = Decider.choose(rules.indices)
      new QuadGenome(rules.updated(
        // choose a random rule
        index,
        rules(index).mutate))
    }
  }
  def crossover(other: QuadGenome): QuadGenome = {
    val crossoverIndex =
      Random.nextInt(math.max(rules.length, other.rules.length))
    new QuadGenome(rules.slice(0, crossoverIndex) ++
        other.rules.slice(crossoverIndex, other.rules.length))
  }

  override def toString: String = s"QuadGenome($rules)"
}
