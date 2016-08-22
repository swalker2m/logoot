package edu.gemini.logoot

import scalaz._

final case class Degree(toInt: Int) extends AnyVal {
  def incr: Degree =
    Degree(toInt + 1)

  def decr: Degree =
    Degree(toInt - 1)

  def isVisible: Boolean =
    toInt > 0

  def isHidden: Boolean =
    !isVisible
}

object Degree {
  val Zero = Degree(0)
  val One  = Degree(1)

  implicit val EqualDegree: Equal[Degree] = Equal.equalA
}
