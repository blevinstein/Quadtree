package com.blevinstein.util

import scala.util.Random

// TODO: make mockable (object -> class?)
// TODO: add test that checks for deviations from expected probabilities
object Decider {
  // Choose random element from a list
  def choose[T](list: Seq[T]): T = list(Random.nextInt(list.length))

  // TODO: handle negative weights
  def chooseWithWeight[T](list: Seq[T], weights: Seq[Float]): T = {
    require(!list.isEmpty)
    require(list.length == weights.length)
    val totalWeight = weights.reduce(_ + _)
    var randomWeight = Random.nextFloat() * totalWeight
    var chosenItem: Option[T] = None
    (list, weights).zipped.foreach { case (item, weight) =>
      if (chosenItem.isEmpty && weight >= randomWeight) {
        chosenItem = Some(item)
      }
      randomWeight = randomWeight - weight
    }
    chosenItem.get
  }

  // Give true with probability p
  def withProb(p: Float): Boolean = Random.nextFloat() < p
}
