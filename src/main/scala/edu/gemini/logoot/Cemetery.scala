package edu.gemini.logoot

import scalaz._
import Scalaz._

/** Cemetery wraps a map from `LineId` to `Degree`, tracking whether the line
  * should appear in the document.  If the `Degree` is positive non-zero, it
  * should appear but otherwise no.
  */
final case class Cemetery(m: LineId ==>> Degree) {

  /** Gets the `Degree` associated with the given `LineId`, defaulting to
    * `Degree.Zero` if not found in the map.
    */
  def get(id: LineId): Degree =
    m.lookup(id) | Degree.Zero

  /** Sets the `Degree` associated with a `LineId`, removing it altogether from
    * the map if `Degree.Zero`.
    */
  def set(id: LineId, d: Degree): Cemetery =
    Cemetery(if (d === Degree.Zero) m - id else m + (id -> d))

  /** Modifies the line `Degree` according to a provided function. */
  def mod(id: LineId)(f: Degree => Degree): Cemetery =
    set(id, f(get(id)))
}

object Cemetery {
  val Empty = Cemetery(==>>.empty[LineId, Degree])

  def fromList(lst: List[(LineId, Degree)]): Cemetery =
    Cemetery(==>>.fromList(lst))
}
