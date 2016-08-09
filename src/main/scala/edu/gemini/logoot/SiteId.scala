package edu.gemini.logoot

import java.util.UUID

import scalaz.Scalaz._
import scalaz._

final case class SiteId(uuid: UUID) extends AnyVal

object SiteId {
  val Min: SiteId =
    SiteId(new UUID(Long.MinValue, Long.MinValue))

  val Max: SiteId =
    SiteId(new UUID(Long.MaxValue, Long.MaxValue))

  implicit val OrderSiteId: Order[SiteId] =
    Order.orderBy { sid =>
      (sid.uuid.getMostSignificantBits, sid.uuid.getLeastSignificantBits)
    }
}
