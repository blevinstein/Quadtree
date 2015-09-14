package com.blevinstein.util

/**
 * When called in a loop, RateLimiter#check returns true AT MOST once every
 * 'millis' milliseconds.
 */
class RateLimiter(millis: Long) {
  var lastMillis: Long = System.currentTimeMillis()

  def check: Boolean = {
    val nowMillis = System.currentTimeMillis()
    if (nowMillis - lastMillis >= millis) {
      lastMillis = nowMillis
      true
    } else {
      false
    }
  }
}
