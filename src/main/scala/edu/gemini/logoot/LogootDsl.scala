package edu.gemini.logoot


import scalaz._

sealed trait LogootDsl[A]

object LogootDsl {
  case object Site           extends LogootDsl[SiteId]
  case object TimeNow        extends LogootDsl[Timestamp]
  case object Tick           extends LogootDsl[Unit]
  case class Rand(n: Number) extends LogootDsl[Number]

  val site            = Free.liftF(Site)
  val timeNow         = Free.liftF(TimeNow)
  val tick            = Free.liftF(Tick)
  def rand(n: Number) = Free.liftF(Rand(n))
}
