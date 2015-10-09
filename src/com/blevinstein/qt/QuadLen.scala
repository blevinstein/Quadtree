package com.blevinstein.qt

/**
 * Represents an exact length in quad coords.
 *
 * length = base * (2^exp)
 */
object QuadLen {
  val zero = new QuadLen(0, 0)
  val one = new QuadLen(1, 0)
  val half = new QuadLen(1, -1)

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

  def toFloat: Float = if (exp >= 0) {
    1f * base * (1 << exp)
  } else {
    1f * base / (1 << -exp)
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
