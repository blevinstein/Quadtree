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
  // TODO: change sign of [resolution]
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
    val quadFloatPart = new QuadLen(min, resolution)

    quadIntPart + (quadFloatPart * math.signum(float).toInt)
  }

  implicit def toFloat(len: QuadLen): Float = len.toFloat

  def min(lens: QuadLen*): QuadLen = lens.reduce((a, b) => if (a <= b) a else b)
  def max(lens: QuadLen*): QuadLen = lens.reduce((a, b) => if (a >= b) a else b)

  // Simple recursive implementation of log2
  def log2(number: Int): Option[Int] = {
    if (number <= 0) {
      None // log2 is only defined on positiven numbers
    } else if (number == 1) {
      Some(0) // Base case
    } else if (number % 2 == 0) {
      // Recursively bitshift to divide by 2
      log2(number >> 1) match {
        case Some(n) => Some(n + 1)
        case None => None
      }
    } else {
      None
    }
  }

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
// TODO: Refactor to be a case class?
class QuadLen(private val base: Int, private val exp: Int = 0) {
  def this(value: Int) = this(value, 0)
  // Returns true if this represents a length of zero. Should be preferred to
  //   quadLen == QuadLen.zero
  //   because QuadLen(0, n) is equivalent to QuadLen(0, 0), but equals() will
  //   return false
  def isZero: Boolean = base == 0

  // Returns the "resolution" of this length, e.g. 3/8 => 1/8 resolution => -3
  def minExp: Int = simplify.exp

  val toFloat: Float = if (exp >= 0) {
    1f * base * (1 << exp)
  } else {
    1f * base / (1 << -exp)
  }
  def +(other: QuadLen): QuadLen = QuadLen.normalize(this, other) match {
    case (a, b, ex) => new QuadLen(a + b, ex)
  }
  def -(other: QuadLen): QuadLen = QuadLen.normalize(this, other) match {
    case (a, b, ex) => new QuadLen(a - b, ex)
  }
  def *(k: Int): QuadLen = new QuadLen(base * k, exp)
  def <<(k: Int): QuadLen = new QuadLen(base, exp + k)
  def >>(k: Int): QuadLen = new QuadLen(base, exp - k)

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

  // Returns the largest QuadLen that is "perfect" and less than or equal to
  // [this].
  def truncatePerfect: QuadLen = {
    require(base > 0, "truncatePerfect only handles positive numbers")
    var perfectNum = new QuadLen(1, exp)
    while (perfectNum * 2 <= this) perfectNum = perfectNum * 2
    perfectNum
  }

  // For "perfect" lengths of the form 1 << x, this will return x
  // NOTE: perfectLog discards sign information
  def perfectLog: Option[Int] =
      if (base > 0) {
        QuadLen.log2(base) match {
          case Some(n) => Some(n + exp)
          case None => None
        }
      } else {
        None
      }

  def isSimplified: Boolean = base % 2 == 1 || base == 0

  def simplify: QuadLen = if (base == 0) {
    QuadLen.zero
  } else if (base % 2 == 0) {
    new QuadLen(base / 2, exp + 1).simplify
  } else {
    this
  }

  override def hashCode: Int = {
    val simplified = simplify
    31 * (simplified.base.hashCode +
      31 * simplified.exp.hashCode)
  }

  override def equals(o: Any): Boolean = o match {
    case other: QuadLen => {
      val simplified = simplify
      val otherSimplified = other.simplify

      simplified.base == otherSimplified.base &&
          simplified.exp == otherSimplified.exp
    }
    case _ => false
  }

  override def toString: String = s"[$toFloat (${base}E${exp})]"
}
