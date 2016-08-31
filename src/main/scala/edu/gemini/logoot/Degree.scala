package edu.gemini.logoot

import scalaz._, Scalaz._

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

  def isVisible: Boolean =
    toInt > 0

  def isHidden: Boolean =
    !isVisible
}

object Degree {
  val Zero = Degree(0)
  val One  = Degree(1)

  implicit val OrderDegree: Order[Degree] =
    Order.orderBy(_.toInt)
}
