package com.blevinstein.qt.grow

import com.blevinstein.ga.Genome
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}
import com.blevinstein.util.Decider

import scala.language.implicitConversions
import scala.util.Random

object QuadGenome {
  // delegate to rules
  implicit def toRuleList(genome: QuadGenome): List[ReplacementRule] =
      genome.rules
}
class QuadGenome(val rules: List[ReplacementRule]) extends Genome[QuadGenome] {
  val growProbability = 0.05f
  def mutate: QuadGenome = {
    if (Decider.withProb(growProbability)) {
      new QuadGenome(rules :+ ReplacementRule.createRandom)
    } else {
      new QuadGenome(rules.updated(
        // choose a random rule
        Decider.choose(rules.indices),
        // replace with mutated version
        ReplacementRule.createRandom))
    }
  }
  def crossover(other: QuadGenome): QuadGenome = {
    val crossoverIndex =
      Random.nextInt(math.max(rules.length, other.rules.length))
    new QuadGenome(rules.slice(0, crossoverIndex) ++
        other.rules.slice(crossoverIndex, other.rules.length))
  }
}
