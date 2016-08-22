package edu.gemini.logoot

import scalaz._

final case class LineIdState(site: SiteId, rng: Rng, timestamp: Timestamp)

object LineIdState {
  def apply(site: SiteId, seed: Long): LineIdState =
    LineIdState(site, Rng(seed), Timestamp.Zero)

  val siteLens: LineIdState @> SiteId =
    Lens.lensu((a, b) => a.copy(site = b), _.site)

  val rngLens: LineIdState @> Rng =
    Lens.lensu((a, b) => a.copy(rng = b), _.rng)

  val timestampLens: LineIdState @> Timestamp =
    Lens.lensu((a, b) => a.copy(timestamp = b), _.timestamp)

  val site: State[LineIdState, SiteId] =
    siteLens.st

  val tick: State[LineIdState, Unit] =
    timestampLens.mods_(_.next)

  val timeNow: State[LineIdState, Timestamp] =
    timestampLens.st

  def rand(n: Number): State[LineIdState, Number] =
    for {
      r1     <- rngLens.st
      (r2, i) = r1.nextNumber(n)
      _      <- rngLens := r2
    } yield i
}

