package edu.gemini.logoot

import edu.gemini.logoot.LogootMessage.Patch

import scalaz._
import scalaz.PLensFamily.somePLens

final case class LogootState[A](doc: LogootDoc[A], session: List[List[LogootOp[A]]], history: HistoryBuffer[A], cemetery: Cemetery, idState: LineIdState)

object LogootState {
  def init[A](site: SiteId, seed: Long): LogootState[A] =
    init[A](LineIdState(site, seed))

  def init[A](lineIdState: LineIdState): LogootState[A] =
    init[A](emptyDoc[A], lineIdState)

  def init[A](doc: LogootDoc[A], lineIdState: LineIdState): LogootState[A] =
    LogootState(doc, List.empty, emptyBuffer[A], Cemetery.Empty, lineIdState)

  def doc[A]: LogootState[A] @> LogootDoc[A] =
    Lens.lensu((a, b) => a.copy(doc = b), _.doc)

  def session[A]: LogootState[A] @> List[List[LogootOp[A]]] =
    Lens.lensu((a, b) => a.copy(session = b), _.session)

  def history[A]: LogootState[A] @> HistoryBuffer[A] =
    Lens.lensu((a, b) => a.copy(history = b), _.history)

  def cemetery[A]: LogootState[A] @> Cemetery =
    Lens.lensu((a, b) => a.copy(cemetery = b), _.cemetery)

  private def scalazMapLens[K, V](k: K)(implicit ev: Order[K]): ==>>[K, V] @> Option[V] =
    LensFamily.lensg(m => {
      case None    => m - k
      case Some(v) => m + (k -> v)
    }: Option[V] => ==>>[K,V], _.lookup(k))

  def patch[A](pid: PatchId): LogootState[A] @?> (Patch[A], Degree) =
    (history[A] andThen scalazMapLens[PatchId, (Patch[A], Degree)](pid)).partial andThen somePLens

  def degree[A](pid: PatchId): LogootState[A] @?> Degree =
    patch[A](pid) andThen LensFamily.secondLens.partial

  def degree[A](lid: LineId): LogootState[A] @> Degree =
    cemetery[A] andThen Lens.lensu[Cemetery, Degree]((c, d) => c.set(lid, d), _.get(lid))

  def idState[A]: LogootState[A] @> LineIdState =
    Lens.lensu((a, b) => a.copy(idState = b), _.idState)

  def site[A]: LogootState[A] @> SiteId =
    idState andThen LineIdState.siteLens
}