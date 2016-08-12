package edu.gemini.logoot

import edu.gemini.logoot.LogootOp.{Patch, Delete, Insert}

/** Logoot algorithm operations. Operations are produced by local edits so that
  * they can be easily shipped to and applied at peers. */
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

  def insert[A](id: LineId, content: A): LogootOp[A] =
    Insert(id, content)

  def delete[A](id: LineId): LogootOp[A] =
    Delete(id)

  def patch[A](ops: List[LogootOp[A]]): LogootOp[A] =
    Patch(ops)
}
