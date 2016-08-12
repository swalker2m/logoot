package edu.gemini.logoot

import scalaz._, Scalaz._

final case class Position(d: Digit, s: SiteId, t: Timestamp) {
  override def toString: String =
    s"<${d.toInt}.${s.uuid.toString.substring(0, 5)}.${t.time}>"
}

object Position {
  val Min = Position(Digit.Zero, SiteId.Min, Timestamp.Zero)
  val Max = Position(Digit.Max,  SiteId.Max, Timestamp.Max)

  implicit val OrderPosition: Order[Position] =
    Order.orderBy(p => (p.d, p.s, p.t))
}
