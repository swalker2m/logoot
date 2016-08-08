package edu.gemini.logoot

import scalaz._, Scalaz._

sealed trait Timestamp {
  def time: Int
  def next: Timestamp

  override def equals(a: Any): Boolean =
    a match {
      case that: Timestamp => time === that.time
      case _               => false
    }

  override def hashCode: Int =
    time
}

object Timestamp {
  def apply(t: Int): Timestamp =
    new Timestamp {
      override val time: Int =
        if (t < 0) 0 else t

      // A little more than 1 billion ticks before wrapping around.
      override def next: Timestamp =
        apply(time + 1)
    }

  val Zero: Timestamp =
    Timestamp(0)

  val Max: Timestamp =
    Timestamp(Int.MaxValue)

  implicit val OrderTimestamp: Order[Timestamp] =
    Order.orderBy(_.time)
}
