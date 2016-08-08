package edu.gemini.logoot

import scalaz._

final case class LogootState(site: SiteId, rng: Rng, timestamp: Timestamp)

object LogootState {
  def apply(site: SiteId, seed: Long): LogootState =
    LogootState(site, Rng(seed), Timestamp.Zero)

  val site: LogootState @> SiteId =
    Lens.lensu((a, b) => a.copy(site = b), _.site)

  val rng: LogootState @> Rng =
    Lens.lensu((a, b) => a.copy(rng = b), _.rng)

  val timestamp: LogootState @> Timestamp =
    Lens.lensu((a, b) => a.copy(timestamp = b), _.timestamp)
}