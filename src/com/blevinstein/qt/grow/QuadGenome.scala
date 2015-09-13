package com.blevinstein.qt.grow

import com.blevinstein.ga.Genome
import com.blevinstein.qt.{QuadTree,QuadBranch,QuadLeaf}

import scala.language.implicitConversions
import scala.util.Random

object QuadGenome {
  // delegate to rules
  implicit def toRuleList(genome: QuadGenome): List[ReplacementRule] =
      genome.rules
}
class QuadGenome(val rules: List[ReplacementRule]) extends Genome[QuadGenome] {
  def mutate: QuadGenome = {
    // choose a random rule
    // replace with mutated version
    this
  }
  def crossover(other: QuadGenome): QuadGenome = {
    val crossoverIndex =
      Random.nextInt(math.max(rules.length, other.rules.length))
    new QuadGenome(rules.slice(0, crossoverIndex) ++
        other.rules.slice(crossoverIndex, other.rules.length))
  }
}
