package edu.gemini.logoot

import scalaz._

final case class LogootState[A](doc: LogootDoc[A], idState: LineIdState)

object LogootState {
  def init[A](site: SiteId, seed: Long): LogootState[A] =
    init[A](LineIdState(site, seed))

  def init[A](lineIdState: LineIdState): LogootState[A] =
    LogootState(emptyDoc[A], lineIdState)

  def idState[A]: LogootState[A] @> LineIdState =
    Lens.lensu((a, b) => a.copy(idState = b), _.idState)

  def site[A]: LogootState[A] @> SiteId =
    idState andThen LineIdState.siteLens

  def doc[A]: LogootState[A] @> LogootDoc[A] =
    Lens.lensu((a, b) => a.copy(doc = b), _.doc)

}