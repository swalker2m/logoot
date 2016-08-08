package edu.gemini.logoot

import LogootDsl._
import scalaz._, Scalaz._

final case class Position(d: Digit, s: SiteId, t: Timestamp) {
  override def toString: String =
    s"<${d.toInt}.${s.uuid.toString.substring(0, 5)}.${t.time}>"
}

object Position {
  val Min = Position(Digit.Zero, SiteId.Min, Timestamp.Zero)
  val Max = Position(Digit.Max,  SiteId.Max, Timestamp.Max)

  def next(d: Digit): Logoot[Position] =
    for {
      sid <- site
      _   <- tick
      t   <- timeNow
    } yield Position(d, sid, t)

  implicit val OrderPosition: Order[Position] =
    Order.orderBy(p => (p.d, p.s, p.t))
}
