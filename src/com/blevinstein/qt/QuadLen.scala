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

  // TODO: find a cleaner way to implement this method
  def approx(float: Float, resolution: Int): QuadLen = {
    val intPart = float.toInt
    val floatPart = math.abs(float % 1) // NOTE: drops sign, adds back at end

    // Bisection method on float part
    var min = 0
    var max = 1 << -resolution
    while (max - min > 1) {
      val mid = (min + max) / 2
      if (new QuadLen(mid, resolution) <= floatPart) {
        min = mid
      } else {
        max = mid
      }
    }
    val quadIntPart = new QuadLen(intPart, 0)
    val quadFloatPart = new QuadLen(min, resolution).simplify

    quadIntPart + (quadFloatPart * math.signum(float).toInt)
  }

  implicit def toFloat(len: QuadLen): Float = len.toFloat

  def min(a: QuadLen, b: QuadLen): QuadLen = if (a <= b) a else b
  def max(a: QuadLen, b: QuadLen): QuadLen = if (a >= b) a else b

  private def normalize(a: QuadLen, b: QuadLen): (Int, Int, Int) = {
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
  // Returns true if this represents a length of zero. Should be preferred to
  //   quadLen == QuadLen.zero
  //   because QuadLen(0, n) is equivalent to QuadLen(0, 0), but equals() will
  //   return false
  def isZero: Boolean = base == 0

  def minExp: Int = simplify.exp

  val toFloat: Float = if (exp >= 0) {
    1f * base * (1 << exp)
  } else {
    1f * base / (1 << -exp)
  }
  def +(other: QuadLen): QuadLen = QuadLen.normalize(this, other) match {
    case (a, b, ex) => new QuadLen(a + b, ex).simplify
  }
  def -(other: QuadLen): QuadLen = QuadLen.normalize(this, other) match {
    case (a, b, ex) => new QuadLen(a - b, ex).simplify
  }
  def *(k: Int): QuadLen = new QuadLen(base * k, exp).simplify
  def <<(k: Int): QuadLen = new QuadLen(base, exp + k).simplify
  def >>(k: Int): QuadLen = new QuadLen(base, exp - k).simplify

  def unary_- : QuadLen = new QuadLen(-base, exp)

  def minExp(other: QuadLen): Int = QuadLen.normalize(this, other) match {
    case (a, b, exp) => exp
  }

  // Delegate comparisons to float
  def >(other: QuadLen): Boolean = toFloat > other.toFloat
  def <(other: QuadLen): Boolean = toFloat < other.toFloat
  def >=(other: QuadLen): Boolean = toFloat >= other.toFloat
  def <=(other: QuadLen): Boolean = toFloat <= other.toFloat

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

  override def toString: String = s"[$base << $exp ~ $toFloat]"
}
