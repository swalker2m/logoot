package edu.gemini.logoot

import scalaz._
import Scalaz._

case class Cemetery(m: LineId ==>> Degree) {
  def get(id: LineId): Degree =
    m.lookup(id) | Degree.Zero

  def set(id: LineId, d: Degree): Cemetery =
    Cemetery(if (d === Degree.Zero) m - id else m + (id -> d))

  def update(id: LineId)(f: Degree => Degree): Cemetery =
    set(id, f(get(id)))
}

object Cemetery {
  val Empty = Cemetery(==>>.empty[LineId, Degree])
}
