package edu.gemini.logoot

import scalaz._, Scalaz._

/** A `Digit` in the base-`Digit.Base` number system.
 */
sealed trait Digit {
  def toInt: Int

  def next: Digit =
    Digit.apply(toInt + 1)

  override def toString: String =
    toInt.toString

  override def equals(a: Any): Boolean =
    a match {
      case d: Digit => d.toInt === toInt
      case _        => false
    }

  override def hashCode: Int =
    toInt
}

object Digit {
  /** The arbitrary `Base` of the number system, which must be at least > 1.
    * The larger the base, the shorter the `Position` ids.
    */
  val Base: Int =
    10

  /** Converts the integer to a `Digit` in the range (0, Base]. */
  def apply(i: Int): Digit =
    new Digit {
      val toInt: Int = (i % Base).abs
    }

  val Zero: Digit =
    Digit(0)

  val One: Digit =
    Digit(1)

  val Max: Digit =
    Digit(Base - 1)

  /** Converts the integer to a `Digit` in the range (0, Base]. Alias for
    * `apply`.
    */
  def fromInt(i: Int): Digit =
    Digit(i)

  implicit val OrderPriority: Order[Digit] =
    Order.orderBy(_.toInt)
}