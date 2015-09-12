package com.blevinstein.ga

import scala.util.Random

trait Genome[G <: Genome[G]] {
  def mutate: G
  def crossover(other: G): G
}

object Population {
  val crossover_rate = 0.7;
  val mutate_rate = 0.05;
}
// TODO: prune members with negative fitness
// TODO: implement tests with mock Random? DecisionModule? (has a random,
//   answers specific questions, e.g. chooseCrossover())
class Population[T <: Genome[T]](members: List[T],
    fitnessFunc: T => Float,
    val generation: Int = 0,
    random: Random = Random) {
  val fitness: List[Float] = members map fitnessFunc
  val totalFitness = fitness reduce (_ + _)

  def evolve: Population[T] = {
    var newMembers = List[T]()
    while (newMembers.length < members.length) {
      newMembers = newMembers :+
        (if (random.nextFloat() < Population.crossover_rate) {
          sample crossover sample // sex
        } else {
          sample // survival
        })
    }
    // Increment generation number
    new Population(newMembers, fitnessFunc, generation + 1)
  }

  def sample: T = {
    // Sample randomly with probability proportional to fitness
    // NOTE: Does not handle negative probabilities
    var randomFitness = random.nextFloat() * totalFitness
    var chosenMember = members.head
    (members, fitness).zipped.foreach { case (m, f) =>
      if (f > randomFitness) {
        chosenMember = m
      }
      randomFitness = randomFitness - f
    }
    // Maybe mutate
    if (random.nextFloat() < Population.mutate_rate) {
      chosenMember.mutate
    } else {
      chosenMember
    }
  }
}

