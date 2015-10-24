package com.blevinstein.qt

import scala.language.implicitConversions

/**
 * Represents an exact length in quad coords.
 *
 * length = base * (2^exp)
 */
object QuadLen {
  val zero = new QuadLen(0, 0)
  val one = new QuadLen(1, 0)
  val half = new QuadLen(1, -1)

  def approx(float: Float, depth: Int): QuadLen = {
    require(0 <= float && float <= 1, "approx only accepts floats in [0, 1]")

    // Bisection method
    var min = 0
    var max = 1 << depth
    while (max - min > 1) {
      val mid = (min + max) / 2
      if (new QuadLen(mid, -depth) <= float) {
        min = mid
      } else {
        max = mid
      }
    }
    new QuadLen(min, -depth).simplify
  }

  implicit def toFloat(len: QuadLen): Float = if (len.exp >= 0) {
    1f * len.base * (1 << len.exp)
  } else {
    1f * len.base / (1 << -len.exp)
  }

  def normalize(a: QuadLen, b: QuadLen): (Int, Int, Int) = {
    val newExp = if (a.base == 0 && b.base == 0) {
      0
    } else if (a.base == 0) {
      b.exp
    } else if (b.base == 0) {
      a.exp
    } else {
      math.min(a.exp, b.exp)
    }
    (a.base << (a.exp - newExp), b.base << (b.exp - newExp), newExp)
  }
}
class QuadLen(private val base: Int, private val exp: Int) {
  def +(other: QuadLen): QuadLen = QuadLen.normalize(this, other) match {
    case (a, b, ex) => new QuadLen(a + b, ex).simplify
  }
  def -(other: QuadLen): QuadLen = QuadLen.normalize(this, other) match {
    case (a, b, ex) => new QuadLen(a - b, ex).simplify
  }
  def *(k: Int): QuadLen = new QuadLen(base * k, exp).simplify
  def <<(k: Int): QuadLen = new QuadLen(base, exp + k)
  def >>(k: Int): QuadLen = new QuadLen(base, exp - k)

  def unary_- : QuadLen = new QuadLen(-base, exp)

  // Delegate comparisons to float
  def >(other: QuadLen): Boolean =
      QuadLen.toFloat(this) > QuadLen.toFloat(other)
  def <(other: QuadLen): Boolean =
      QuadLen.toFloat(this) < QuadLen.toFloat(other)
  def >=(other: QuadLen): Boolean =
      QuadLen.toFloat(this) >= QuadLen.toFloat(other)
  def <=(other: QuadLen): Boolean =
      QuadLen.toFloat(this) <= QuadLen.toFloat(other)

  def untilBy(until: QuadLen, by: QuadLen): List[QuadLen] = {
    var result = List[QuadLen]()
    var current = this
    while (current < until) {
      result = current :: result
      current += by
    }
    result
  }

  // For "perfect" lengths of the form 1 << x, this will return x
  def perfectLog: Option[Int] = base match {
    case 1 => Some(exp)
    case _ => None
  }

  def isSimplified: Boolean = base % 2 == 1 || base == 0

  private def simplify: QuadLen = if (base == 0) {
    QuadLen.zero
  } else if (base % 2 == 0) {
    new QuadLen(base / 2, exp + 1).simplify
  } else {
    this
  }

  override def hashCode: Int =
    31 * (base.hashCode +
      31 * exp.hashCode)

  override def equals(o: Any): Boolean = o match {
    case other: QuadLen => base == other.base && exp == other.exp
    case _ => false
  }

  override def toString: String = s"[$base << $exp]"
}
