package edu.gemini.logoot

import java.util.UUID
import scalaz._, Scalaz._

final case class PatchId(uuid: UUID) extends AnyVal

object PatchId {
  def random: PatchId =
    PatchId(UUID.randomUUID)

  implicit val OrderPatchId: Order[PatchId] = Order.orderBy { pid =>
    (pid.uuid.getMostSignificantBits, pid.uuid.getLeastSignificantBits)
  }
}