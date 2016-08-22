package edu.gemini.logoot

import edu.gemini.logoot.LineId.{End, Beginning}

import scalaz._, Scalaz._
import scalaz.Ordering.{EQ, LT, GT}

/** The id of a particular element in the document. */
sealed trait LineId {
  /** Determines whether this line id is a special marker of the beginning or
    * end of the document.
    * @return true if the beginning or end, false otehrwise
    */
  def isSpecial: Boolean = this match {
    case Beginning | End => true
    case _               => false
  }

  /** Determines whether this is a "normal" middle (between beginning and end)
    * line id.
    * @return true if a middle id, false otherwise
    */
  def isMiddle: Boolean =
    !isSpecial
}

object LineId {
  /** A phantom id that is sorted before all other possible ids. */
  case object Beginning                                      extends LineId

  /** An id of an element in the document. */
  final case class Middle(positions: NonEmptyList[Position]) extends LineId

  /** A phantom id that is sorted after all other possible ids. */
  case object End                                            extends LineId

  def middle(head: Position, tail: List[Position]): LineId =
    Middle(NonEmptyList(head, tail: _*))

  implicit val OrderMiddle: Order[Middle] = Order.order { (id0, id1) =>
    // Add Zero positions to the end of the shorter position list. These
    // always sort before any other position.  If the two position lists share
    // the same prefix, the shorter one sorts before.
    val z = id0.positions.toList.zipAll(id1.positions.toList, Position.Min, Position.Min)
    z.dropWhile { case (a, b) => a === b } match {
      case Nil         => EQ
      case (a, b) :: _ => Order[Position].order(a, b)
    }
  }

  implicit val OrderElementId: Order[LineId] = Order.order { (id0, id1) =>
    (id0, id1) match {
      case (m0: Middle, m1: Middle) => OrderMiddle.order(m0, m1)
      case (Beginning, Beginning)   => EQ
      case (End, End)               => EQ
      case (Beginning, _)           => LT
      case (_, Beginning)           => GT
      case (_, End)                 => LT
      case (End, _)                 => GT
    }
  }

}
