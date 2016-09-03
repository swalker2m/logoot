package edu.gemini.logoot

import scalaz._, Scalaz._

/** Count of number of times a `Patch` or particular line has been applied or
  * undone.  A positive number indicates that the patch should have an effect
  * while a zero or negative number means that it should not.
  *
  * @param toInt
  */
final case class Degree(toInt: Int) extends AnyVal {
  def incr: Degree =
    this + Degree.One

  def decr: Degree =
    this - Degree.One

  def +(that: Degree): Degree =
    Degree(toInt + that.toInt)

  def -(that: Degree): Degree =
    Degree(toInt - that.toInt)

  def unary_-(): Degree =
    Degree(-toInt)

  /** Determines whether the corresponding patch or element should be included
    * in the document.
    */
  def isVisible: Boolean =
    toInt > 0

  /** A `Degree` is hidden if not visible. */
  def isHidden: Boolean =
    !isVisible
}

object Degree {
  val Zero = Degree(0)
  val One  = Degree(1)

  implicit val OrderDegree: Order[Degree] =
    Order.orderBy(_.toInt)

  implicit val MonoidDegree: Monoid[Degree] =
    Monoid.instance((d0, d1) => d0 + d1, Zero)
}
