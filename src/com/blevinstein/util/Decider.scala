package com.blevinstein.util

import scala.util.Random

// TODO: make mockable (object -> class?)
// TODO: add test that checks for deviations from expected probabilities
object Decider {
  // Choose random element from a list
  def choose[T](list: List[T]): T = list.apply(Random.nextInt(list.length))

  // TODO: handle negative probabilities
  def chooseWithWeight[T](list: List[T], weights: List[Float]): T = {
    require(list.length == weights.length)
    val totalWeight = weights.reduce(_ + _)
    var randomWeight = Random.nextFloat() * totalWeight
    var chosenItem = list.head
    (list, weights).zipped.foreach { case (item, weight) =>
      if (weight > randomWeight) {
        chosenItem = item
      }
      randomWeight = randomWeight - weight
    }
    chosenItem
  }

  // Give true with probability p
  def withProb(p: Float): Boolean = Random.nextFloat() < p
}
