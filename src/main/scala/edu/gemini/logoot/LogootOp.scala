package edu.gemini.logoot

import edu.gemini.logoot.LogootOp.{Patch, Delete, Insert}

/** Logoot algorithm operations. */
sealed trait LogootOp[A] {
  def apply(doc: LogootDoc[A]): LogootDoc[A] =
    this match {
      case Insert(id, a) => doc + (id -> a)
      case Delete(id)    => doc - id
      case Patch(ops)    => (doc/:ops) { (d,op) => op(d) }
    }
}

object LogootOp {
  /** Inserts the content associated with the given id into the document. */
  case class Insert[A](id: LineId, content: A) extends LogootOp[A]

  /** Deletes the content associated with the given id from the document. */
  case class Delete[A](id: LineId) extends LogootOp[A]

  /** Combines multiple operations into one. */
  case class Patch[A](ops: List[LogootOp[A]]) extends LogootOp[A]
}
