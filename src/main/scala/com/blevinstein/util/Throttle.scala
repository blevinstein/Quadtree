package com.blevinstein.util

// sleep timing logic inspired by Processing
// https://code.google.com/p/processing/source/browse/trunk/processing/android/core/src/processing/core/PApplet.java?r=7046

// USAGE
// val throttle = new Throttle(100)
// while (true) {
//    doSomething()
//    throttle.sleep
// }

class Throttle(rate : Int) {
  val BILLION = 1000000000L
  val MILLION = 1000000L
  // NOTE: all times are in nanoseconds
  var beforeTime = System.nanoTime()
  var oversleep = 0L

  def sleep : Unit = {
    // determine duration between sleep() calls
    val afterTime = System.nanoTime()
    val duration = afterTime - beforeTime

    // determine how long to wait
    val waitTime = BILLION / rate - duration - oversleep
    if (waitTime> 0) {
      try {
        Thread.sleep(waitTime / MILLION)
      } catch {
        case e : InterruptedException => ()
      }
    }
    val lastTime = System.nanoTime()

    // calculate oversleep, total of all unaccounted time in last cycle
    oversleep = lastTime - (beforeTime + duration + waitTime)

    // update beforeTime for next call to sleep
    beforeTime = lastTime
  }
}
