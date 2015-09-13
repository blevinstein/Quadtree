package com.blevinstein.ga

import com.blevinstein.util.Decider

trait Genome[G <: Genome[G]] {
  def mutate: G
  def crossover(other: G): G
}

object Population {
  val crossover_rate = 0.7f;
  val mutate_rate = 0.05f;
}
// TODO: prune members with negative fitness
class Population[T <: Genome[T]](members: List[T],
    fitnessFunc: T => Float,
    val generation: Int = 0) {
  val fitness: List[Float] = members map fitnessFunc

  def evolve: Population[T] = {
    var newMembers = List[T]()
    while (newMembers.length < members.length) {
      newMembers = newMembers :+
        (if (Decider.withProb(Population.crossover_rate)) {
          sample crossover sample // sex
        } else {
          sample // survival
        })
    }
    // Increment generation number
    new Population(newMembers, fitnessFunc, generation + 1)
  }

  def sample: T = {
    val chosenMember = Decider.chooseWithWeight(members, fitness)
    // Maybe mutate
    if (Decider.withProb(Population.mutate_rate)) {
      chosenMember.mutate
    } else {
      chosenMember
    }
  }
}

